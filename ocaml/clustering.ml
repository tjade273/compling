#use "alignment.ml";;
  
type 'a tree = Leaf of 'a token list | Node of (('a tree * 'a tree)  * float * 'a alignment)

                                                 (*
let print_tree = function
  | Leaf x -> print_string x; print_string "; "
  | Node _ -> ()
                               

                                                  *)       
let distance score s1 s2 = (1.-.(2.*.(score s1 s2)/.((score s1 s1) +. (score s2 s2))))  

let rec cardinality = function
  | Leaf _ -> 1
  | Node ((a,  b), _,_) -> (cardinality a)+(cardinality b)

let rec nest_map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd tl, hd):: nest_map f tl

 
                            
let pop_min f l =
  let rec find_min v m  =
    function
    | [] -> (v, m)
    | hd::tl -> (if f hd < v then find_min (f hd) hd else find_min v m) tl 
  in
  let v, m = find_min max_int (List.hd l) l in
  ((v, m), List.remove l m)

let rec upgma (score : 'a token list -> 'a token list -> float)
              (merge : 'a alignment -> 'a alignment -> int -> int ->  'a alignment)
              (cutoff : float) (clusters : 'a tree list) =
  let rec score_nodes a b : float =
    match a, b with
    | Leaf a, Leaf b -> score a b
    | Leaf _, Node _ -> score_nodes b a
    | Node ((x,y), _, _) , _ ->
       let hx, hy = float_of_int (cardinality x), float_of_int (cardinality y) in 
       (hx*.(score_nodes x b)+.hy*.(score_nodes y b))/.(hx+.hy)
  in
  let rec merge_nodes a b =
    let ha, hb = cardinality a, cardinality b in 
    let alignify (t : 'a token list) = List.map (fun x -> Token [x]) t in 
    match (a, b) with
    | Leaf x, Leaf y -> merge (alignify x) (alignify y) ha hb
    | Leaf x, Node (_,_, y) -> merge (alignify x) y ha hb 
    | Node (_,_,x) , Leaf y -> merge x (alignify y) ha hb
    | Node (_,_,x) , Node (_,_,y) -> merge x y ha hb 
  in 
  match clusters with
  | [] -> []
  | _::[] -> clusters
  | _ -> 
     let ((s,x), y) = List.min (
                          nest_map (fun hd tl ->
                              List.min (
                                  (infinity, Leaf [])::List.map (fun t ->
                                      (score_nodes hd t, t)
                                    ) tl)
                            ) clusters)
     in
     if s > cutoff then clusters else
       upgma score merge cutoff (Node((x,y),s/.2., merge_nodes x y)::(List.remove (List.remove clusters x) y))
                       
                       
let distance_score = distance (fun x y -> snd (global_align x y)) 

let memoize f  n  =
  let hashmap = Hashtbl.create n in
  fun x y -> 
  match Hashtbl.find_option hashmap (x,y) with
  | None -> Hashtbl.add hashmap (x, y) (f x y); Hashtbl.find hashmap (x,y)
  | Some s -> s 
    
let upgma_wrapper score merge cutoff l =
  upgma score merge cutoff (List.map (fun n -> Leaf (
                                                   List.map (fun x -> Token x)
                                                            (String.explode n)
                                                 )
                                     ) l)

        
(*
let test = upgma_wrapper (memoize distance_score 100) (-0.1) ["ABCD"; "ACD"; "XXYYZ"; "XXXYYZ"; "PQRST"; "PRSTT";"ABCEE"; "1234"; "16182872"; "157626781"; "1778879"; "9jkjkiu"];;*)
        
let test1 =
  upgma_wrapper distance_score
                          (merge_align global_score)
                          (0.5) ["AB";"ABCEFD"; "AAAAABBBC";"ABCABC"; "XYZ"; "XYZAB"; "XXXXXXXXXXXXXXXXXYYYY"];;
