(* a, b, c, d cases test *)

module Cases = struct
  module VSet = Graph.VSet
  module ESet = Graph.ESet
  open Print
  open MoreGraph

  let case_a graph couplage tree =
    let even_v = NTree.even_vertices tree in
    let compute_solutions vertex =
      VSet.diff
        (Graph.out_neighbours vertex graph)
        (VSet.union
           (saturated_vertices couplage)
           (NTree.vset tree))
    in
    let solutions =
      List.fold_left
        (fun accu vertex ->
           ESet.union
             (Conversion.eset_of_vset vertex (compute_solutions vertex))
             accu)
        ESet.empty even_v
    in
    let choose = ESet.choose in
    print_eset solutions;Printf.printf "\n";
    let chosen =
      if ESet.is_empty solutions then None
      else
        (print_arc (choose solutions); Printf.printf "\n";
         Some(choose solutions))
    in
    chosen

  let case_b graph couplage tree =
    let even_v = NTree.even_vertices tree in
    let compute_ys vertex =
      VSet.diff
        (Graph.out_neighbours vertex graph)
        (VSet.union
           (NTree.vset tree)
           (couplage_neighboors_of vertex couplage))
    in
    let compute_zs vertex =
      VSet.diff
        (VSet.inter
           (Graph.out_neighbours vertex graph)
           (couplage_neighboors_of vertex couplage))
        (VSet.add vertex (NTree.vset tree))
    in
    let blabla f lst=
      List.fold_left (fun accu x -> (f x)@accu) [] lst
    in
    let for_each_y ys =
      blabla (fun y -> (List.map (fun z -> (y,z))
                          (VSet.elements (compute_zs y)))) ys
    in
    let for_each_x xs =
      blabla (fun x -> (List.map (fun (y,z) -> (x,y,z))
                          (for_each_y (VSet.elements (compute_ys x))) )) xs
    in
    let solutions = for_each_x even_v in
    let choose = List.hd in
    print_list (fun (x,y,z) -> Printf.printf "(%d,%d,%d)" x y z) solutions;
    Printf.printf "\n";
    let chosen =
      if solutions = [] then None
      else
        let (x,y,z) as sol = choose solutions in
        Printf.printf "(%d,%d,%d)" x y z; Some(sol)
    in
    chosen

  let case_c graph couplage tree =
    let even_v = NTree.even_vertices tree in
    let neighboors_in_tree_edge_not_in vertex =
      VSet.diff
        (VSet.inter
           (VSet.remove vertex (VSet.of_list even_v))
           (Graph.out_neighbours vertex graph))
        (couplage_neighboors_of vertex couplage)
    in
    let solutions =
      List.fold_left (fun accu vertex ->
          (List.map (fun elt -> (vertex,elt))
             (VSet.elements (neighboors_in_tree_edge_not_in vertex)))@accu)
        [] even_v
    in
    let choose = List.hd in
    print_arc_list solutions; Printf.printf "\n";
    let chosen =
      if solutions = [] then None
      else
        let sol = choose solutions in
        print_arc sol; Printf.printf "\n"; Some(sol)
    in
    chosen
end

module BlossomAlgo = struct
  module VSet = Graph.VSet
  module ESet = Graph.ESet
  open Print
  open Conversion
  open MoreGraph

  let anti_arc (x,y) = (y,x)
  let anti_arcs = List.map anti_arc
  let anti_arcs_eset eset = ESet.of_list (anti_arcs (ESet.elements eset))
  let unoriented_arcs_eset eset =
    ESet.union eset (anti_arcs_eset eset)
  let unoriented_arcs lst =
    ESet.elements (unoriented_arcs_eset (ESet.of_list lst))

  (* blossom management *)

  let find_blossom edge tree =
    let blossom = NTree.path_from_to (fst edge) (snd edge) tree in
    if blossom <> [] then blossom else
      NTree.path_from_to (snd edge) (fst edge) tree

  let find_new_vertex_name graph =
    (Graph.fold_vertices ~f:(fun x y -> if (x > y) then x else y) graph (-999999)) + 1

  let contract_graph blossom graph meta_vertex=
    let extract f =
      List.fold_left
        (fun accu vertex -> ESet.union (f vertex graph) accu)
        ESet.empty
        (blossom)
    in
    let replace_vertex x meta = if List.mem x blossom then meta else x in
    let update_arcs arcs meta_vertex =
      ESet.fold
        (fun (x, y) accu ->
           ESet.add
             (replace_vertex x meta_vertex,
              replace_vertex y meta_vertex)
             accu)
        arcs
        ESet.empty
    in
    let meta_in_arcs =
      update_arcs (extract Graph.delta_in) meta_vertex in
    let meta_out_arcs =
      update_arcs (extract Graph.delta_out) meta_vertex in
    let arcs =
      ESet.union meta_out_arcs meta_in_arcs in
    let cleaned_graph =
      remove_vertices_from_graph
        (Graph.add_vertex
           meta_vertex
           graph)
        blossom
    in
    ESet.fold
      (fun (src, dst) accu ->
         (* Est-ce quon peut avoir un graphe orienté ?????? add_arc ou add_edge???? *)
         Graph.add_edge src dst  accu)
      (ESet.remove (meta_vertex, meta_vertex) arcs)
      cleaned_graph

  let min_depth_vertex lst tree =
    let solution =
      List.fold_left
        (fun a b -> if (snd a) < (snd b) then a else b) (-1,99999999)
        (List.map (fun elt -> (elt, NTree.depth_of elt tree)) lst)
    in
    fst solution

  let remove_min_depth_vertex_from_blossom blossom tree =
    List.filter (fun elt -> elt <> (min_depth_vertex blossom tree)) blossom

  let to_contract blossom tree =
    remove_min_depth_vertex_from_blossom blossom tree

  let contract to_contract tree =
    List.fold_left
      (fun accu vertex -> NTree.contract vertex accu) tree to_contract

  let rename old_vertex new_vertex tree =
    let rec my_fun = function
      | NTree.Node(node_value, node_childs) ->
        if old_vertex == node_value then NTree.Node(new_vertex, node_childs)
        else NTree.Node(node_value, List.map (fun node -> my_fun node) node_childs)
    in
    if not (NTree.mem old_vertex tree) then
      tree
    else
      my_fun tree

  let contract_tree meta_vertex blossom tree=
    rename
      (min_depth_vertex blossom tree)
      meta_vertex (contract (to_contract blossom tree) tree)

  let contract_couplage meta_vertex blossom couplage =
    let update_vertex v = if List.mem v blossom then meta_vertex else v in
    let updated_couplage =
      ESet.fold (fun (x, y) accu -> ESet.add (update_vertex x, update_vertex y) accu)
      couplage ESet.empty
    in
    ESet.filter (fun (x, y) -> x <> y) updated_couplage

  let add_path_to_couplage couplage tree last =
    ESet.union
      (ESet.of_list (NTree.even_arcs_to last tree))
      (ESet.diff
         couplage
         (ESet.of_list (unoriented_arcs (NTree.uneven_arcs_to last tree))))


  let init_node graph couplage =
    let solutions = unsaturated_vertices graph couplage in
    let chosen =
      VSet.choose solutions
    in
    NTree.Node(chosen, [])

  (* Actions for cases *)

  let rec test_case_a graph couplage tree =
    Printf.printf "\n";
    Printf.printf "Tree : "; print_tree tree; Printf.printf "\n";
    Printf.printf "Cas A : On cherche (x,y)
avec x pair & x et y nApp (couplage U tree)\n";
    match Cases.case_a graph couplage tree with
    | Some (edge)->
      (graph, add_path_to_couplage couplage (NTree.add edge tree) (snd edge))
    | None ->
      test_case_b graph couplage tree

  and test_case_b graph couplage tree =
    Printf.printf "Cas B : On cherche (x,y)(y,z)
avec x pair & y et z nApp tree & (x,y) nApp couplage & (y,z) app couplage\n";
    match Cases.case_b graph couplage tree with
    | Some (x, y, z)->
      test_case_a
        graph
        couplage
        (NTree.add (y, z) (NTree.add (x, y) tree))
    | None ->
      test_case_c graph couplage tree

  and update_couplage meta_vertex blossom graph couplage =
      let update_arc (x,y) =
        let a y =
          (VSet.inter
             (ESet.fold
                (fun arc accu -> VSet.add (fst arc) accu)
                (Graph.delta_in y graph) VSet.empty)
             (VSet.of_list blossom))
        in
        if x = meta_vertex then
          (VSet.choose (a y), y)
        else if y = meta_vertex then
          (Printf.printf "updating_arcs for matching original graph and there is an arc (X, meta_vertex) and I am not sure whether this is possible or what to do ?!?!?!?!";
          (x, VSet.choose(a x)))
        else
          (x,y)
      in
      let updated_couplage =
        ESet.fold (fun arc accu -> ESet.add (update_arc arc) accu)
          couplage ESet.empty
      in
      let (new_new_contracted_graph, new_updated_couplage) =
        blossom_algorithm (graph, updated_couplage)
      in
      Printf.printf "Graph :\n"; Print.print_delta_out graph;
      Printf.printf "Contracted_graph :\n"; Print.print_delta_out new_new_contracted_graph;
      Printf.printf "New couplage :\n"; Print.print_eset new_updated_couplage; Printf.printf "\n";
      (new_new_contracted_graph, new_updated_couplage)

  and test_case_c graph couplage tree =
    Printf.printf "Cas C : on cherche (x, y)
avec x et y pair & (x,y) nApp tree\n";
    match Cases.case_c graph couplage tree with
    | Some (edge)->
      let meta_vertex = find_new_vertex_name graph in
      let blossom = find_blossom edge tree in
      let contracted_graph = contract_graph blossom graph meta_vertex in
      let contracted_tree = contract_tree meta_vertex blossom tree in
      let contracted_couplage = contract_couplage meta_vertex blossom couplage in
      Printf.printf "blossom : "; print_int_list blossom; Printf.printf "\n";
      Print.print_delta_out contracted_graph; Printf.printf "\n";
      Print.print_tree contracted_tree; Printf.printf "\n";
      Print.print_eset contracted_couplage; Printf.printf "\n";
      let (_, new_couplage) =
        test_case_a contracted_graph contracted_couplage contracted_tree
      in
      update_couplage meta_vertex blossom graph new_couplage
    | None ->
      test_case_d graph couplage tree

  and test_case_d graph couplage tree =
    Printf.printf "Cas D\n";
    (remove_tree_from_graph graph tree, couplage)

  (* Blossom algorithm *)

  and blossom_algorithm (graph, couplage) =
    let nb_insature = VSet.cardinal (unsaturated_vertices graph couplage) in
    Printf.printf "\n\nCouplage actuel : ";
    print_eset couplage;
    Printf.printf "Nombre de sommets insaturés : %d\n" nb_insature ;
    if nb_insature >= 2 then
      begin
        Printf.printf "On construit un arbre :\n" ;
        test_case_a graph couplage (init_node graph couplage)
        |> blossom_algorithm
      end
    else
      begin
        Printf.printf "Plus assez de sommets insaturés\nOn arrete\n";
        (graph, couplage)
      end

  (* Initialise l'algorithme *)
  let init_algorithm graph =
    Random.self_init ();
    Printf.printf "Initialisation\n" ;
    (graph, ESet.empty)

  let do_blossom graph =
    graph
    |> init_algorithm
    |> blossom_algorithm
end

(* Normal Graph *)
let graph_list1 = [(1,8);(1,2);(1,5);
                  (2,1);(2,8);(2,3);
                  (3,5);(3,10);(3,2);(3,9);(3,6);
                  (4,5);(4,7);(4,6);
                  (5,1);(5,3);(5,4);(5,7);
                  (6,4);(6,7);(6,9);(6,3);
                  (7,5);(7,4);(7,6);
                  (8,1);(8,2);
                  (9,3);(9,6); (10,3)]

(* Odd number of vertices *)
let graph_list2 = [(1,2);(1,3);
                  (2,1);(2,4);
                  (3,1);(3,4);(3,5);
                  (4,2);(4,3);(4,6);(4,7);
                  (5,3);(5,6);
                  (6,4);(6,5);(6,7);
                  (7,4);(7,6)]

(* Graph with a blossom *)
let graph_list3 = [(1,2);(1,4);
                   (2,1);(2,3);(2,5);
                   (3,2);(3,5);
                   (4,1);
                   (5,2);(5,3);(5,6);
                   (6,5)]

let graph = Conversion.graph_of_list graph_list3

let _ = BlossomAlgo.do_blossom graph

let _ = ()


(*let my_tree_list =
  [(1,2);(1,3);(1,4);
   (2,5);(2,6);(2,7);
   (3,8);(3,9);(3,10);
   (4,11);(4,12);(4,13);
   (5,14);(5,15);(5,16);
   (6,17);(6,18);(6,19);
   (7,20);(7,21);(7,22);
   (8,23);(8,24);(8,25);
   (9,26);(9,27);(9,28)]

  let my_tree = NTree.of_arcs my_tree_list
  let my_blossom = [13;4;12]*)


(*let _ = VSet.elements (saturated_vertices couplage)             (* 1,2,3,6,8,9 *)
  let _ = VSet.elements (unsaturated_vertices graph couplage)     (* 4,5,7,10 *)
  let _ = ESet.elements (NTree.eset tree)                       (* [(5,7);(7,4)] *)
  let _ = VSet.elements (NTree.vset tree)                       (* 5,7,4,6,8 *)
  let _ = NTree.even_vertices tree                      (* 5,8,4 *)
  let _ = NTree.uneven_vertices tree                    (* 6,7 *)

  let _ = NTree.even_arcs_to 8 tree
  let _ = NTree.uneven_arcs_to 8 tree*)

(*let blossom_edge = match  case_c graph couplage tree with | Some(edge) -> edge | None -> (0,0)
  let my_blossom = find_blossom blossom_edge tree
  let new_tree = remove_blossom_from_tree blossom_edge tree
  let new_graph = contract_blossom_in_graph my_blossom graph*)

(*let _ = print_tree tree*)

(*let x =
  Graph.empty
  |> Graph.add_arc ~src:1 ~dst:2
  |> Graph.add_arc ~src:2 ~dst:1
  |> Graph.remove_vertex 2
  let _ = print_delta_in x
  let _ = print_delta_out x*)

(*
let list = [(1,2);(1,3);(1,4);(1,5);
            (2,6);(2,7);(2,8);(2,9);
            (3,10);(3,11);(3,12);(3,13);
            (4,14);(4,15);(4,16);(4,17);
            (5,18);(5,19);(5,20);(5,21);
            (6,22);(6,23);(6,24);(6,25);
            (11,26);(11,27);(11,28)]
let tree = Tree.of_arcs list
open NTree
let _ = value tree (* 1 *)
let _ = childs tree (* 5 4 3 2 *)
let _ = find 5 tree (* Node (5, [21, 20, 19, 18]) *)
let _ = vertices tree (* 1 2 3 4 ... 28 *)
let _ = arcs tree  (* (1,2) (2,6) (6,22) ... (2,7) ... (5,21) *)
let _ = path_to 28 tree (* 28 11 3 1 *)
let _ = path_from_to 3 26 tree (* 3 11 26 *)
let _ = even_vertices tree (* 1 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 *)
let _ = uneven_vertices tree (* 2 22 23 24 25 3 26 27 28 4 5 *)
let _ = even_arcs tree (* (1,2) (6,22...)) (1,3) (11, 26...) (1,4) (1,5) *)
let _ = uneven_arcs tree (* (2,6...) (3,10...) (4,14...) (5,18...) *)
let _ = even_path_to 28 tree (* 11 1 *)
let _ = uneven_path_to 28 tree (* 28 3 *)
let _ = even_arcs_to 28 tree (* 28,11 3,1*)
let _ = uneven_arcs_to 28 tree (* 11,3 *)
let _ = depth tree
let _ = size tree
let a = match find 28 tree with | Some (a) ->a | None -> create_node 0
let _ = is_leaf a
let _ = add (27, 28) tree (* TODO if mem (fst edge) then tree else add *)
let _ = Graph.ESet.elements (eset tree)
let _ = Graph.VSet.elements (vset tree)
let _ = iter (function node -> print_int (value node); Printf.printf " ") tree
let _ = print tree
let _ = create_node 5
let _ = mem (29) tree
let _ = add (18,1) tree
let _ = add (0,100) tree
let _ = add (16,30) tree
let _ = remove 5 tree
*)


(*let list = [(1,2);(2,3);(2,13);(3,4);(4,5);(4,6);(3,7);(7,8);(8,10);(7,9);(9,14);(9,11);(9,12);(12,15)]
  let tree = Tree.of_arcs list
  let path = [2;3;7]
  let a = Tree.find_vertices path tree
  let _ = List.map (fun node -> Tree.remove_vertices path  node) a
  let _ = Tree.contract_path (-1) path tree*)
