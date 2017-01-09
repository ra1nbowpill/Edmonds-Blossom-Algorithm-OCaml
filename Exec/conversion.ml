(* Conversions *)

let rec vset_of_eset edges =
  Graph.ESet.fold
    (fun (x, y) accu -> Graph.VSet.add x (Graph.VSet.add y accu))
    edges
    Graph.VSet.empty
let eset_of_vset vertex vertices =
  Graph.VSet.fold
    (fun elt accu -> Graph.ESet.add (vertex, elt) accu)
    vertices
    Graph.ESet.empty
let couple_of_list fst list =
  List.map (fun a -> (fst, a)) list
let vset_of_list vertices =
  List.fold_right Graph.VSet.add vertices Graph.VSet.empty
let eset_of_list edges =
  List.fold_right Graph.ESet.add edges Graph.ESet.empty
let graph_of_list edges =
  List.fold_right (fun (x, y) accu -> Graph.add_edge x y accu) edges Graph.empty
let tree_of_list edges =
  List.fold_left (fun tree edge -> NTree.add edge tree) (NTree.Node(fst (List.hd edges), [])) edges
