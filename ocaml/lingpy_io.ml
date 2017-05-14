       
type 'a ipa_token = ('a * string) token
type 'a ipa_sequence = 'a ipa_token list
type 'a psq =
  string * (string * (string * 'a ipa_sequence)*(string * 'a ipa_sequence) BatEnum.t)

let str_of_seq s =
    String.join "\t" (List.map (function Gap -> "-" | Handle -> "+" | Token(_, c) -> c) s) 
       
let read_psq (filename : string) (model : string -> 'a token) =
  let lines = File.lines_of filename in
  let description = Option.get (BatEnum.get lines) in
  let pairwise_alignments = BatEnum.from_while (fun () ->
                                if BatEnum.count lines >= 3 then
                                  Some (BatEnum.take 3 (BatEnum.take 4 lines))
                                else None)
  in
  let pair_of_enum e =
    let label = Option.get(BatEnum.get e) in
    let l1, p1 = String.split (Option.get (BatEnum.get e)) "\t" in
    let l2, p2 = String.split (Option.get (BatEnum.get e)) "\t" in
    let l1, l2, p1, p2 = String.strip l1, String.strip l2,
                         String.strip p1, String.strip p2 in
    (label,
     (l1, List.map model (String.split_on_char '.' p1 )),
     (l2, List.map model (String.split_on_char '.' p2 ))
    )
  in
  (description, BatEnum.map pair_of_enum pairwise_alignments)

let psq_align f p =
  let align_pair (label, (l1, p1), (l2, p2)) =
    let (a1, a2),_ = f p1 p2 in
    (label, (l1, a1), (l2, a2))
  in
  (fst p, BatEnum.map align_pair (snd p))


let print_psa filename (title, alignments) =
  let file_string (desc, (l1, p1), (l2,p2)) =
    desc^"\n"
    ^l1^"\t"^(str_of_seq p1)^"\n"
    ^l2^"\t"^(str_of_seq p2)^"\n"
  in
  let lines = BatEnum.map file_string alignments in
  BatEnum.push lines title; File.write_lines filename lines

type 'a msq = (string * string) *  'a tree list
type 'a msa = (string * string) * ('a token list * string) list 

let read_msq (filename : string) (model : string -> 'a token) : 'a msq =
  let lines = File.lines_of filename in
  let dataset = Option.get (BatEnum.get lines) in
  let phrase = Option.get (BatEnum.get lines) in
  let parse_string s =
    let (a,b) = String.split s "\t" in
     Leaf (List.map (fun t -> model t) (String.split_on_char '.' b), a)
  in
  let alignments = BatEnum.map parse_string lines in
  ((dataset,phrase), List.of_enum alignments)
                               
let msq_align score align ((d,p), a) : 'a msa=
  let sim x y = snd (align x y) in
  let dist = distance sim in 
  let detokenize = function Token a -> a | _ -> failwith "Bad final alignment" in
  let upgma_tree = List.hd (upgma dist (merge_align score) infinity a) in
  match upgma_tree with
  | Node (_,_,alignment, names) -> ((d,p),
                                    List.map2 (fun a b -> (a,b))
                                              (List.transpose
                                                 (List.map detokenize alignment))
                                              names)
  | _ -> failwith "Clustering Error"

let print_msa filename ((d,p), a) =
  let header = d^"\n"^p in
  let file_string (l, n) = n^"\t"^(str_of_seq l) in
  File.write_lines filename  (List.enum (header::(List.map file_string a)))
              
(*            
let p = read_psq "/home/tjaden/Documents/compling/project/data/Online_Resource_1/I_Gold_Standard/reference.psq" dolgo_of_string
let a = psq_align dolgo_align p
let () = print_psa "/home/tjaden/Desktop/testalign.psa" a;;
 *)

let run_msa_test model score align msqs msas n =
   let readdir_option dir =
    try
      Some (Unix.readdir dir)
    with
      End_of_file -> None
   in
   let d = Unix.opendir msqs in
   let files = BatEnum.from_while (fun () -> readdir_option d) in
   let files = List.of_enum (BatEnum.take n (BatEnum.filter
                               (fun s -> String.ends_with s ".msq") files))
   in
   print_int (List.length files);print_newline ();
   let run_alignment f =
     print_string f; print_newline ();
     let m = read_msq (msqs^"/"^f) model in
     let l = List.length (snd m) in 
     let a = msq_align score (memoize align (l*l*l*2) ) m in
     print_msa (msas^"/"^ (String.slice ~last:(-3) f)^"msa") a
   in
   List.iter run_alignment files;
   Unix.closedir d
               

(*
let msas = "../data/test_data/";;
let msqs = "../data/Online_Resource_2/I_Gold_Standard/msq/";;
let () = run_msa_test dolgo_of_string dolgo_score dolgo_align msqs msas 100;;
 *)
