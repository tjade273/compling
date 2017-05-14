type 'a wordlist  = string * 'a msq list 
                                                                   
let read_wordlist filename (model : string -> 'a token) : 'a wordlist=
  let lines = File.lines_of filename in
  let header = (List.of_enum (BatEnum.take 3 lines)) in
  let header = match header with  x1::x2::x3::[] -> x1^"\n"^x2^"\tLexStatID\n"^x3 in 
  let parse_string (s : string) : 'a tree =
    let split_string = String.split_on_char '\t' s in
    let tokenized_ipa = List.nth split_string 5 in
    let ipa = List.map model (String.split_on_char ' ' tokenized_ipa) in
    Leaf(ipa, s)
  in
  let cognate_groups = BatEnum.filter (fun e -> (Option.get (Enum.peek e)).[0] != '#')
                                      (BatEnum.group (fun s -> s.[0] = '#') lines) in
  let cognate_groups = BatEnum.map (fun e -> ("",""),
                                             List.of_enum (
                                                 BatEnum.map parse_string e
                                               )
                                   ) cognate_groups in 
  let cognate_groups = List.of_enum cognate_groups in
  (header, cognate_groups)
                                     
   
let align_wordlist score align cutoff (header, (msqs : 'a msq list)) : string * 'a msa list list =
  let multi_align (_, a) : 'a msa list =
    let sim x y = snd (align x y)in
    let dist = distance sim in 
    let tree_to_msa t = 
      match t  with
      | Node (_,_,alignment, names) -> ("",""), List.map (fun n -> [], n) names
      | Leaf (_, n) -> ("",""), [([], n)]
    in
    let dont_merge a b h1 h2 = [Token [] ]  in 
    List.map tree_to_msa (upgma dist dont_merge cutoff a) 
  in
  header, (List.map multi_align msqs)


let write_alignments filename ((header, msas) : string * 'a msa list list) =
  let file_string i (_, n)  = n^"\t"^(string_of_int i) in
  let msas = List.flatten msas in
  let lines = List.enum (header::(List.flatten (
                           (List.mapi
                              (fun i l -> List.map (file_string i) l)
                              (List.map snd  msas)
                           ))))         
  in
  File.write_lines filename lines

(*                   
let n = 2.
let path = "../data/SequenceComparison-SupplementaryMaterial-cc4bf85/benchmark/cognates/GER.csv"
let wlist = read_wordlist path dolgo_of_string
let aligned = align_wordlist dolgo_score dolgo_align n wlist
let () =  write_alignments "../cogtest/test1.csv" aligned
 *)

type 'a score_matrix = ((string * string) * (('a token * 'a token) * int) list) list
                   
let create_pairwise_scores (alignments : 'a tree list) : 'a score_matrix =
  let update_or_init l ((n1,r1),(n2,r2)) =
    List.modify_def [((r1,r2), 1)] (n1,n2) (List.modify_def 1 (r1,r2) ((+) 1)) l
  in
  let combine_mats (l1 : 'a score_matrix) (l2 : 'a score_matrix) : 'a score_matrix =
    List.map (fun (k, v) -> k,
        if List.mem_assoc k l2 then
          let l = List.assoc k l2 in
          List.map (fun (k1, v1) ->
              (k1 , v1 + (if List.mem_assoc k1 l then
                     (List.assoc k1 l)
                          else 0))) v
        else v
      ) l1
  in      
  let rec pairwise_alignments =
    function
    | Leaf _ -> []
    | Node((child1, child2), _, a, name) ->
       let pair_column =
         function Token c -> 
                  let labeled_column = List.combine name c in
                  let pairs = List.cartesian_product labeled_column labeled_column in
                  List.fold_left update_or_init [] pairs
       in
       combine_mats (List.fold_left combine_mats [] (List.map pair_column a))
                    (combine_mats (pairwise_alignments child1)
                                  (pairwise_alignments child2)
                    )
  in
  List.fold_left combine_mats [] (List.map pairwise_alignments alignments)
                  
let create_random_scores (words : ('a token list * string) list) n: 'a score_matrix =
  let shuffle list =
    let n = 100 * (List.length list) in
    List.map snd (List.sort compare (List.map (fun x -> (Random.int n, x)) list))
  in
  let pairs = List.take n (shuffle (List.cartesian_product words words)) in
  let aligned_pairs = (* TO DO: align pairs, build tree list, create scoring matrix *)
