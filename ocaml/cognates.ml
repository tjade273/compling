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
                                     
   
let align_wordlist score align cutoff a (header, (msqs : 'a msq list)) : string * 'a msa list =
  let multi_align (_, alignment) : 'a msa list =
    let sim l1 l2 x y = snd (align l1 l2 x y)in
    let dist = distance sim in
    let detokenize = function Token a -> a | _ -> failwith "Bad final alignment" in
    let fake_tree_to_msa = function 
      | Node (_,_,alignment, names) -> ("",""), List.map (fun n -> [], n) names
      | Leaf (_, n) -> ("",""), [([], n)]
    in
    let t2msa =
      function 
      | Node (_,_,alignment, names) -> (("",""),
                                        List.map2 (fun a b -> (a,b))
                                                  (List.transpose
                                                     (List.map detokenize alignment))
                                                  names)
      | _ -> (("",""),[])
    in
    let tree_to_msa = if a then t2msa else fake_tree_to_msa in 
    let dont_merge a b h1 h2 = [Token [] ]  in
    let merge = if a then merge_align score else dont_merge in 
    List.map tree_to_msa (upgma dist merge cutoff alignment)
  in
  header, (List.flatten (List.map multi_align msqs))


let write_alignments filename ((header, msas) : string * 'a msa list) =
  let file_string i (_, n)  = n^"\t"^(string_of_int i) in
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
let aligned = align_wordlist dolgo_score dolgo_align 0.8 false wlist 
let () =  write_alignments "../test2.csv" aligned
 *)
type 'a score_matrix = ((string * string) * (('a token * 'a token) * float) list) list



let combine_mats (l1 : 'a score_matrix) (l2 : 'a score_matrix) : 'a score_matrix =
  List.fold_left (fun acc (k, v) ->
      List.modify_def v k (fun l ->
                        List.fold_left (fun acc (k1,v1) ->
                            List.modify_def v1 k1 ((+.) v1) acc) [] (l@v)) acc
    ) [] (l1@l2)
                        

let normalize_mat m =
  let normalize_pair (n, p)  =
    let total = List.fold_left (fun acc (_,v) -> acc +. v) 0. p in
    (n, List.map (fun (k,v) -> (k, v/. total)) p)
  in List.map normalize_pair m

let create_pairwise_scores (alignments : ('a alignment * string list) list) : 'a score_matrix =
  let update_or_init l ((n1,r1),(n2,r2)) =
    List.modify_def [((r1,r2),1.)]
                    (n1,n2) (List.modify_def 1. (r1,r2)
                                             ((+.) 1.)) l
  in
  let  pairwise_alignments (a, name) =
       let pair_column =
         function Token c -> 
                  let labeled_column = List.combine name c in
                  let pairs =
                    List.filter (fun ((n1,_), (n2, _)) -> n1 != n2)
                                (List.cartesian_product
                                   labeled_column labeled_column
                                ) in
                  List.fold_left update_or_init [] pairs
       in
       List.fold_left combine_mats [] (List.map pair_column a)               
  in
  List.fold_left combine_mats [] (List.map pairwise_alignments alignments)
                 
let rec create_random_scores align (words : ('a token list * string) list) n: 'a score_matrix =
  let shuffle list =
    print_string ("Shuffling: "^(string_of_int n));print_newline();
    let n = 100 * (List.length list) in
    List.map snd (List.sort compare (List.map (fun x -> (Random.int n, x)) list))
  in
  let mkalignment w1 w2 =
    List.map (fun x-> Token x) (List.transpose ((fun(a,b)->[a;b])(fst(align w1 w2))))
  in
  let shuffled = shuffle words in 
  let pairs = List.take n (List.filter (fun ((_,n1), (_, n2)) -> n1 != n2)
                          (List.cartesian_product shuffled shuffled)) in
  let aligned_pairs = List.map (fun ((w1, n1),(w2, n2)) ->
                          mkalignment w1 w2, [n1;n2]) pairs
  in
  (create_pairwise_scores aligned_pairs)

let random_scores align words = normalize_mat (List.fold_left combine_mats [] (List.map (create_random_scores align words) (List.make 50 20)))
                
let language_name_of_data data =
  List.nth (String.split_on_char '\t' data) 1

let list_of_words ((_, l) : 'a wordlist) : ('a token list * string) list = 
  let stripped_msqs = List.flatten (List.map snd l) in
  let rec get_leaves  =
    function
    | Leaf (t, n) -> [(t,language_name_of_data n)]
    | Node((l1, l2), _,_, _) ->  List.append (get_leaves l1) (get_leaves l2)
  in
  List.flatten (List.map get_leaves stripped_msqs)
               
let scores_of_aligned_wordlist (_, alist) =
  let clean (_, strlist) =
    List.map (fun (t,n) -> (t, language_name_of_data n)) strlist
  in
  let to_alignment (tlist : 'a token list list) : 'a alignment = List.map (fun x-> Token x) (List.transpose tlist) in 
  let msalist = List.map clean alist in
  let tokens = List.map (List.map fst) msalist in
  let names = List.map (List.map snd) msalist  in
  let alignments = List.map to_alignment tokens in
  normalize_mat (create_pairwise_scores (List.combine alignments names))

let create_scores r1 r2 score unexpected unattested attested expected l1 l2 t1 t2 c=
  let att = if List.mem_assoc (l1,l2) attested then
              if List.mem_assoc (t1,t2) (List.assoc (l1,l2) attested) then
                List.assoc (t1,t2) (List.assoc (l1,l2) attested)
              else unattested
            else unattested in 
  let expect = if List.mem_assoc (l1,l2) expected then
              if List.mem_assoc (t1,t2) (List.assoc (l1,l2) expected) then
                List.assoc (t1,t2) (List.assoc (l1,l2) expected)
              else unexpected
            else unexpected
  in
  (((r1/. log 2.)  *. (log (att**2.)/.(expect**2.))) +. (r2*. score t1 t2 c))/.(r1+.r2)

let dolgo_no_lang = dolgo_align "" "" 
                                                                                
let make_dolgo wlist =
  let words = list_of_words wlist in
  let expected = random_scores dolgo_no_lang words in
  let aligned = align_wordlist dolgo_score dolgo_align 6. true wlist in
  let attested = scores_of_aligned_wordlist aligned in 
  let pair_scores l1 l2 = create_scores 2. 2. dolgo_score (-5.) 0.00000001 attested expected (language_name_of_data l1) (language_name_of_data l2) in
  let lexalign l1 l2=
    align
      (fun (i1, j1, s1) (i2, j2, s2) -> (max i1 i2, max j1 j2))
      [] (pair_scores l1 l2)
  in
  align_wordlist dolgo_score lexalign 25. false wlist

let test s =
  let w = read_wordlist (path^s) dolgo_of_string in
  let m = make_dolgo w in
  write_alignments ("../"^s) m

let langs = ["GER.csv"; "SLV.csv"; "JAP.csv"; "ROM.csv"]
(*
let () = List.iter test langs 

 *)
