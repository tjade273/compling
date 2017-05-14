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

                   
let n = 2.
let path = "../data/SequenceComparison-SupplementaryMaterial-cc4bf85/benchmark/cognates/GER.csv"
let wlist = read_wordlist path dolgo_of_string
let aligned = align_wordlist dolgo_score dolgo_align n wlist
let () =  write_alignments "../cogtest/test1.csv" aligned

                          
