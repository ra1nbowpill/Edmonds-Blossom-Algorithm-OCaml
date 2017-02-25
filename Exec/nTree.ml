module Dijkstra = struct
  (* Thanks to https://rosettacode.org/wiki/Dijkstra%27s_algorithm#OCaml *)

  let list_vertices graph =
    List.fold_left (fun acc ((a, b), _) ->
        let acc = if List.mem b acc then acc else b::acc in
        let acc = if List.mem a acc then acc else a::acc in
        acc
      ) [] graph

  let neighbors v =
    List.fold_left (fun acc ((a, b), d) ->
        if a = v then (b, d)::acc else acc
      ) []

  let remove_from v lst =
    let rec aux acc = function [] -> failwith "remove_from"
                             | x::xs -> if x = v then List.rev_append acc xs else aux (x::acc) xs
    in aux [] lst

  let with_smallest_distance q dist =
    match q with
    | [] -> assert false
    | x::xs ->
      let rec aux distance v = function
        | x::xs ->
          let d = Hashtbl.find dist x in
          if d < distance
          then aux d x xs
          else aux distance v xs
        | [] -> (v, distance)
      in
      aux (Hashtbl.find dist x) x xs

  let dijkstra max_val zero add graph source target =
    let vertices = list_vertices graph in
    let dist_between u v =
      try List.assoc (u, v) graph
      with _ -> zero
    in
    let dist = Hashtbl.create 1 in
    let previous = Hashtbl.create 1 in
    List.iter (fun v ->                  (* initializations *)
        Hashtbl.add dist v max_val         (* unknown distance function from source to v *)
      ) vertices;
    Hashtbl.replace dist source zero;    (* distance from source to source *)
    let rec loop = function
      | [] -> ()
      | q ->
        let u, dist_u =
          with_smallest_distance q dist in   (* vertex in q with smallest distance in dist *)
        if dist_u = max_val then
          failwith "vertices inaccessible";  (* all remaining vertices are inaccessible from source *)
        if u = target then () else begin
          let q = remove_from u q in
          List.iter (fun (v, d) ->
              if List.mem v q then begin
                let alt = add dist_u (dist_between u v) in
                let dist_v = Hashtbl.find dist v in
                if alt < dist_v then begin       (* relax (u,v,a) *)
                  Hashtbl.replace dist v alt;
                  Hashtbl.replace previous v u;  (* previous node in optimal path from source *)
                end
              end
            ) (neighbors u graph);
          loop q
        end
    in
    loop vertices;
    let s = ref [] in
    let u = ref target in
    while Hashtbl.mem previous !u do
      s := !u :: !s;
      u := Hashtbl.find previous !u
    done;
    (source :: !s)

  let dijkstra_int = dijkstra max_int 0 (+)

end

(* Arbitrary tree *)

type t =
  | Node of (Graph.vertex * t list)

(* Getters *)

let value = function Node(v, _) -> v

let childs = function Node(_, childs) -> childs

(* Tree management *)

let depth_of vertex =
  let rec my_fun depth =
    function
    | Node(value, []) -> if value = vertex then 1 + depth else -1
    | Node(value, childs) ->
      if value = vertex then 1 + depth
      else List.fold_left (fun accu node -> max accu (my_fun (depth+1) node)) (-1) childs
  in
  my_fun (-1)

let rec depth = function
  | Node(v, childs) ->
    1 + List.fold_left (fun accu node -> max accu (depth node)) (-1) childs

let rec size = function
  | Node(v, childs) ->
    List.fold_left (fun accu node -> accu + (size node)) 1 childs

let is_leaf tree = (childs tree) = []

let rec find vertex = function node ->
  if (value node) == vertex then Some(node)
  else List.fold_left
      (fun accu node -> match accu with
         | Some(a) -> Some(a)
         | None -> find vertex node)
      None (childs node)

let unsafe_find vertex tree =
  match find vertex tree with
  | Some(a) -> a
  | None -> failwith "Not_Found"

let rec mem vertex = function node ->
  if (value node) == vertex then true
  else List.fold_left
      (fun accu node ->
         if not accu then mem vertex node
         else accu)
      false (childs node)

let rec add (x, y) tree =
  let my_fun (x, y) = function
    | Node(v, childs) ->
      if v == x then Node(v, (Node(y, []))::childs)
      else Node(v, List.map (fun node -> add (x, y) node) childs)
  in
  if not (mem x tree) || (mem y tree) then
    tree
  else
    my_fun (x, y) tree

let contract vertex tree =
  let rec my_fun = function Node(node_value, node_childs) ->
    let new_list = List.fold_left
        (fun new_list node ->
           if (value node) = vertex then (childs node)@new_list
           else (my_fun node)::new_list)
        [] node_childs
    in
    Node(node_value, new_list)
  in
  if not (mem vertex tree) then
    tree
  else
  if (value tree) = vertex then tree
  else my_fun tree

let remove vertex tree =
  let rec my_fun = function
    | Node(node_value, node_childs) ->
      let new_list = List.fold_left
          (fun new_list node ->
             if (value node) = vertex then new_list
             else (my_fun node)::new_list)
          [] node_childs
      in
      Node(node_value, new_list)
  in
  if not (mem vertex tree) then
    tree
  else
  if (value tree) = vertex then failwith "Cannot remove root from tree"
  else my_fun tree

let anti_arc (x,y) = (y,x)
let anti_arcs = List.map anti_arc
let anti_arcs_eset eset = Graph.ESet.of_list (anti_arcs (Graph.ESet.elements eset))

let unoriented_arcs_eset eset =
  Graph.ESet.union eset (anti_arcs_eset eset)
let unoriented_arcs lst =
  Graph.ESet.elements (unoriented_arcs_eset (Graph.ESet.of_list lst))

let find_vertices vertices tree =
  List.fold_left (fun accu vertex -> (unsafe_find vertex tree)::accu) [] vertices

let remove_vertices vertices tree =
  List.fold_left (fun accu vertex -> remove vertex accu) tree vertices

let rec contract_path name path = function node ->
  Node(value node, List.map
         (fun node ->
            if value node = List.hd path then
              let ma_liste = List.map (remove_vertices path) (find_vertices path node) in
              Node(name, List.fold_left (fun accu node -> (childs node)@accu) [] ma_liste)
            else
              contract_path name path node
         ) (childs node))

(* Functions *)

let rec vertices = function node ->
  (value node)::List.fold_left (fun accu node -> (vertices node)@accu) [] (childs node)

let rec arcs = function
  | Node(node_value, node_childs) ->
    List.fold_left
      (fun accu node ->
         (node_value, value node)::(arcs node)@accu)
      []
      node_childs

let path_from_to src dst tree =
  let graph = List.map (fun elt -> (elt, 1)) (unoriented_arcs (arcs tree)) in
  Dijkstra.dijkstra_int graph src dst
let path_to dst tree =
  path_from_to (value tree) dst tree

let old_path_to vertex =
  let rec my_fun accu = function
    | Node(node_value, node_childs) ->
      if node_value = vertex then node_value::accu
      else
        List.fold_left
          (fun ac no ->
             if ac = [] then
               (my_fun (node_value::accu) no)
             else ac)
          [] node_childs
  in
  my_fun []

let old_path_from_to src dst tree =
  match find src tree with
  | None -> []
  | Some(node) -> let a = old_path_to dst node in
    if a = [] then [] else path_to dst node

(* Getters with parity *)

let rec p_vertices parity = function Node(v, childs) ->
  let res =
    List.fold_left (fun accu node -> (p_vertices (not parity) node)@accu) [] childs
  in
  if parity then v::res
  else res

let rec p_arcs parity = function
  | Node(node_value, node_childs) ->
    let my_fun =
      if parity then
        (fun accu node -> (node_value, value node)::(p_arcs (not parity) node)@accu)
      else
        (fun accu node -> (p_arcs (not parity) node)@accu)
    in
    List.fold_left my_fun [] node_childs

let p_path_to parity vertex  =
  let rec my_fun accu parity = function
    | Node(node_value, node_childs) ->
      let aaa elt lst parity =
        if parity then elt::lst else lst
      in
      if node_value = vertex then
        aaa node_value accu parity
      else
        List.fold_left
          (fun ac no -> (my_fun (aaa node_value accu parity) (not parity) no)@ac)
          [] node_childs
  in
  my_fun [] parity

let even_vertices = p_vertices true
let uneven_vertices = p_vertices false

let even_arcs = p_arcs true
let uneven_arcs = p_arcs false

let even_path_to = p_path_to true
let uneven_path_to = p_path_to false

let rec p_arcs_of_path parite = function
  | [] -> []
  | [elt] -> []
  | elt1::elt2::next ->
    if parite then
      (elt1, elt2)::p_arcs_of_path (not parite) (elt2::next)
    else
      p_arcs_of_path (not parite) (elt2::next)

let even_arcs_to dst tree =
  p_arcs_of_path true (path_to dst tree)
let uneven_arcs_to dst tree =
  p_arcs_of_path false (path_to dst tree)

(* Conversions *)

let rec eset tree=
  Graph.ESet.of_list (arcs tree)

let rec vset tree =
  Graph.VSet.of_list (vertices tree)

let rec iter f = function
    node -> f node;
    List.iter (iter f) (childs node)

(* Constructors *)

let empty = Node(-1, [])
let create_node v lst = Node(v, lst)
let of_arcs lst =
  List.fold_left (fun tree arc -> add arc tree) (create_node (fst (List.hd lst)) []) lst
let of_eset arcs = of_arcs (Graph.ESet.elements arcs)
