type node = int

type edge = node * int option

type graph = (node * edge list) list 

let empty_graph () : graph = []

let add_node g n : graph =
  if List.exists (fun (x, _) -> x = n) g then g
  else (n, []) :: g


(* ajouter une arete non valuée *)
let add_edge g a b : graph =
  let rec add a b = function
    | (x, l) :: xs when x = a -> (x, (b, None) :: l) :: xs
    | y :: ys -> y :: add a b ys
    | [] -> failwith "Node not found"
  in
  add a b g

(* ajouter une arete valuée *)
let add_weighted_edge g a b w : graph =
  let rec add a b w = function
    | (x, l) :: xs when x = a -> (x, (b, Some w) :: l) :: xs
    | y :: ys -> y :: add a b w ys
    | [] -> failwith "Node not found"
  in
  add a b w g


  