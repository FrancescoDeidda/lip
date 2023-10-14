let rec lang1 l1 = 
  match l1 with
[] -> false
| ['0'] -> true
| ['1'] -> true
| '0' :: t -> lang1 t
| '1' :: t -> lang1 t
| _ :: _ -> false;;

(*        0?1*        *)
let rec lang2 l2 = 
  match l2 with
  [] -> true
  | ['0'] -> false
  | ['1'] -> true
  | '0' :: t -> lang2 t
  | '1' :: t -> lang2 t
  | _ :: _ -> false

(*   0[01]*0    *)
let rec lang3 l3 = 
  match l3 with
  [] -> false
  | ['0'] -> false
  | ['1'] -> false
  | '0' :: ['0'] -> true
  | '1' :: ['0'] -> true
  | '0' :: t -> lang3 t
  | '1' :: t -> lang3 t
  | _ :: _ -> false

(*   0*10*10*   *)
let rec lang4 l4 = 
  match l4 with
  [] -> false
  | ['1'] -> false
  | ['0'] -> false
  | '0' :: t -> if t = '1' :: t then lang4 t else false
  | '1' :: t -> if t = '1' :: t then lang4 t else true
  | _ :: _ -> false

let lang5 l5 = match l5 with
'0' :: t -> if t = '0' :: [] then true 
|'1' :: t -> if t = '1' :: [] then true 
| _ :: _ -> false 
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
