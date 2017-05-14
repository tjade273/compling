type 'a token = Gap | Handle | Token of 'a

type backpointer = Null | Match | Insert | Delete

type 'a alignment = 'a token list token list

let print_token = function | Gap -> '-'| Handle -> '+' | Token x -> x

let print_backpointer p =
  let s = match p with
    | Null -> "⟳ "
    | Delete -> "↑  "
    | Insert -> "← "
    | Match -> "↖ "
  in print_string s


let rec print_array a =
    match a with
    | [] -> print_endline "";
    | hd::tl -> print_char (print_token hd); print_array tl
                            
           
let align startpoint default_cell score s1 s2 =
  let l1 = List.length s1 in
  let l2 = List.length s2 in
  let s1 = Handle::s1 in
  let s2 = Handle::s2 in
  let max_cells cells=  
    let max_cell (n1, l1) (n2, l2) =
      match compare n1 n2 with
      | 1 -> (n1, l1)
      | -1 -> (n2, l2)
      | 0 -> (n1, List.append l1 l2)
      | _ -> failwith "Compare Error"
    in
    List.fold_left max_cell (neg_infinity, []) cells
  in
  let mat = Array.make_matrix (l1+1) (l2+1) (0., [Null]) in
  let cg i j = (List.hd (snd mat.(i).(j))) = Insert
               || (List.hd (snd mat.(i).(j))) = Delete in
  let best_cell = ref (0,0) in 
  for i = 0 to l1 do
    (*  print_char '|';*)
    for j = 0 to l2 do
      let adjacent_cells = default_cell in
      let adjacent_cells = if i > 0 then
                             (fst mat.(i-1).(j) +.
                                (score (List.nth s1 (i)) Gap (cg (i-1) j)),
                              [Delete])::adjacent_cells
                           else adjacent_cells in
      let adjacent_cells = if j > 0 then
                             (fst mat.(i).(j-1) +.
                                (score  Gap (List.nth s2 (j)) (cg i (j-1))),
                              [Insert])::adjacent_cells
                           else adjacent_cells in
      let adjacent_cells = if (j > 0) && (i > 0) then
                             (fst mat.(i-1).(j-1) +.
                                (score (List.nth s1 (i)) (List.nth s2 (j))
                                       (cg (i-1) (j-1))),
                              [Match])::adjacent_cells
                           else adjacent_cells
      in
      
      (if i+j != 0 then 
         mat.(i).(j) <- max_cells adjacent_cells
       else ()
      );
      (* print_backpointer (List.hd (snd mat.(i).(j)));
      if (fst mat.(i).(j)) >= 0 then( print_char ' ';
         print_int (fst mat.(i).(j)); print_char ' ';*)
      best_cell :=  startpoint (i, j, (fst mat.(i).(j)))
                    (fst !best_cell,
                     snd !best_cell,
                     fst(mat.(fst !best_cell).(snd !best_cell)));
    done;
    (*   print_endline "|"*)
  done;
  let rec calculate_alignment a1 a2 i j =
    (*    print_backpointer (List.hd (snd mat.(i).(j))); print_endline "";print_array a1; print_array a2; print_endline "";*)
    match List.hd (snd mat.(i).(j)) with
    | Match -> calculate_alignment ((List.nth s1 (i))::a1)
                                   ((List.nth s2 (j))::a2)
                                   (i-1) (j-1)
    | Delete -> calculate_alignment ((List.nth s1 i)::a1) (Gap::a2)
                                    (i-1) j
                                    
    | Insert -> calculate_alignment  (Gap::a1) ((List.nth s2 j)::a2)
                                     i (j-1)
    | Null -> (a1, a2)
  in
  (calculate_alignment [] [] (fst !best_cell) (snd !best_cell),
   (fst mat.(fst !best_cell).(snd !best_cell)))

let global_score a b c =
  match (a,b) with
  | (Handle, Handle) -> 0.
  | _ -> if a = b then 1. else -1.
                                    
                                        
let global_align =
  align
    (fun (i1, j1, s1) (i2, j2, s2) -> (max i1 i2, max j1 j2))
    [] global_score
  

let local_align =
  let local_score a b c =
    match (a, b) with
    | Handle, _
      | _, Handle -> 0.
    | _, Gap 
      | Gap, _ ->  if c then 0. else -4.
    |_ ->  if a = b then 4. else -4.
  in align
       (fun (i1, j1, s1) (i2, j2, s2) ->
         if s2 > s1 then (i2,j2) else (i1,j1))
       [(0., [Null])]
       local_score

let wrapper f s1 s2 =
  let apply_to f ((x,y),s) = ((f x, f y), s) in 
  apply_to (fun x -> String.of_list (List.map print_token x))
           (f  (List.map (fun x-> Token x) (String.explode s1))
               (List.map (fun x-> Token x) (String.explode s2))
           )

                  
let nw = fun() -> (wrapper global_align "GATTACA"  "GCATGCU")
                   
let sw = fun() -> (wrapper local_align "TGTTACGG" "GGTTGACTA")

let merge_align (score : 'a token -> 'a token -> bool -> float) s1 s2 h1 h2=
  let rec multiscore (t1 : 'a token list token) (t2 : 'a token list token) c =
    match (t1, t2) with
    | ((Handle | Gap) as a) , ((Handle | Gap) as b) -> score a b c
    | Token _, (Handle | Gap) -> multiscore t2 t1 c
    | ((Handle | Gap) as a), Token t -> (List.fold_left (fun acc x -> acc+.(score a x c)) 0. t) /. float_of_int (List.length t)
    | Token a, Token b -> (List.fold_left
                            (fun acc ai -> (List.fold_left 
                                              (fun acc bi -> acc +. score ai bi c)
                                              0. b) +. acc)
                            0. a)/. float_of_int ((List.length a)*(List.length b))
                            
                                  
  in
  let stack (s1 : 'a alignment) (s2 : 'a alignment) : 'a alignment =
    let combine_tokens t1 t2 = 
      let t1 = match t1 with
        | Gap -> List.make h1 Gap
        | Token t -> t
      in
      let t2 = match t2 with
        | Gap -> List.make h2 Gap
        | Token t -> t
      in
      Token(t1@t2)
    in List.map2 combine_tokens  (s1) (s2)
  in
  let ((x,y), _) = align  (fun (i1, j1, s1) (i2, j2, s2) -> (max i1 i2, max j1 j2))
                          [] multiscore s1 s2 in
  let res =  stack x y in
  res
    
let print_align a =
  let at = List.map (function Token x -> x) a in 
  (List.map (fun x -> String.of_list (List.map print_token x)) (List.transpose at))
