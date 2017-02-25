module VSet = Graph.VSet
module ESet = Graph.ESet
(* Graph management *)
let remove_vertices_from_graph graph vertices =
  List.fold_left
    (fun accu vertex -> Graph.remove_vertex vertex accu)
    graph
    vertices
let remove_vset_from_graph graph vset =
  let vertices = VSet.elements vset in
  remove_vertices_from_graph graph vertices
let remove_tree_from_graph graph tree =
  let vertices = NTree.vertices tree in
  remove_vertices_from_graph graph vertices

(* Matching & Graph getters *)
let saturated_vertices matching =
  ESet.fold (fun (x, y) accu -> VSet.add x (VSet.add y accu))
    matching
    VSet.empty
let unsaturated_vertices graph matching =
  VSet.diff (Graph.vertices graph) (saturated_vertices matching)
let matching_neighboors_of vertex matching =
  ESet.fold
    (fun (x, y) accu ->
       if x == vertex then VSet.add y accu
       else if y == vertex then VSet.add x accu
       else accu)
    matching
    VSet.empty
