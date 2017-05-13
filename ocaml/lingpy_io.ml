
type 'a ipa_token = ('a * string) token
type 'a ipa_sequence = 'a ipa_token list
type 'a psq =
  string * (string * (string * 'a ipa_sequence)*(string * 'a ipa_sequence) BatEnum.t)
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
  let str_of_seq s =
    String.join "\t" (List.map (function Gap -> "-" | Handle -> "+" | Token(_, c) -> c) s) in
  let file_string (desc, (l1, p1), (l2,p2)) =
    desc^"\n"
    ^l1^"\t"^(str_of_seq p1)^"\n"
    ^l2^"\t"^(str_of_seq p2)^"\n"
  in
  let lines = BatEnum.map file_string alignments in
  BatEnum.push lines title; File.write_lines filename lines

let p = read_psq "/home/tjaden/Documents/compling/project/data/Online_Resource_1/I_Gold_Standard/reference.psq" dolgo_of_string
let a = psq_align dolgo_align p
let () = print_psa "/home/tjaden/Desktop/testalign.psa" a;;

