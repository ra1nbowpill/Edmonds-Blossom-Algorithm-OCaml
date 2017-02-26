(* a, b, c, d cases test *)

module Self =
struct
  type t = {
    layout : Graph.vertex -> Gg.p2;
    graph : Graph.t;         (* Le graphe d'origine *)
    matching : Graph.ESet.t;          (* L'arbre couvrant *)
    tree : NTree.t;
    blossom : Graph.vertex list;
  }

  let empty_strct = {
    layout = (fun a -> Gg.P2.o);
    graph = Graph.empty;
    matching = Graph.ESet.empty;
    tree = NTree.empty;
    blossom = [];
  }

  let rec mem_matching vertex set =
    let rec xx = function
      | (a, b)::next ->
        if vertex == a || vertex == b then true
        else xx next
      | [] -> false
    in
    xx (Graph.ESet.elements set)

  let rec arcs_of_vertices = function
    | v1::v2::next -> (v1, v2)::(arcs_of_vertices (v2::next))
    | v::[] -> []
    | [] -> []

  let arcs_of_vertices_looped vertices =
    if vertices == [] then [] else
    let rec last = function
      | elt::[] -> elt
      | n::ext -> last ext
      | [] -> failwith "oupsi"
    in
    let arcs = arcs_of_vertices vertices in
    (List.hd vertices, last vertices)::arcs

  let vertex_properties strct vertex =
    let open MoreImage in
    if List.mem vertex (NTree.even_vertices strct.tree) then
      Properties.([drawing_color (Gg.Color.v_srgbi 63 169 242); (*bleu*)
                   linewidth (0.03);])
    else if List.mem vertex (NTree.uneven_vertices strct.tree) then
      Properties.([drawing_color (Gg.Color.v_srgbi 95 197 64); (*vert*)
                   linewidth (0.03);])
    else
      Properties.([drawing_color (Gg.Color.black)])


  let arc_properties strct ((src, dst) as arc) =
    let open MoreImage in
    let blossom = arcs_of_vertices_looped strct.blossom in
    if List.mem arc blossom
        || List.mem (dst, src) blossom then
      Properties.([
          drawing_color (Gg.Color.green);
          draw_triangle (false);
        ])
    else if Graph.ESet.mem arc strct.matching
        || Graph.ESet.mem (dst, src) strct.matching then
      Properties.([
          drawing_color (Gg.Color.red);
          draw_triangle (false);
        ])
    else
      Properties.([
          draw_triangle (false);
        ])


  let to_image ctxt =
    MoreImage.draw_graph
      ~vertex_properties:(vertex_properties ctxt)
      ~arc_properties:(arc_properties ctxt)
      ~layout:ctxt.layout
      ctxt.graph
end

type t = Self.t
open Self
module Show = JsContext.AddDrawable(Self)

let show_blossom_context ctxt =
  JsContext.compose
    (Show.show ~width:120 ~height:90 ctxt)
    (JsContext.end_line)

module Cases = struct
  module VSet = Graph.VSet
  module ESet = Graph.ESet
  open MoreGraph

  let case_a graph matching tree =
    let even_v = NTree.even_vertices tree in
    let compute_solutions vertex =
      VSet.diff
        (Graph.out_neighbours vertex graph)
        (VSet.union
           (saturated_vertices matching)
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
    let chosen =
      if ESet.is_empty solutions then None
      else
         Some(choose solutions)
    in
    chosen

  let case_b graph matching tree =
    let even_v = NTree.even_vertices tree in
    let compute_ys vertex =
      VSet.diff
        (Graph.out_neighbours vertex graph)
        (VSet.union
           (NTree.vset tree)
           (matching_neighboors_of vertex matching))
    in
    let compute_zs vertex =
      VSet.diff
        (VSet.inter
           (Graph.out_neighbours vertex graph)
           (matching_neighboors_of vertex matching))
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
    let chosen =
      if solutions = [] then None
      else
        let (x,y,z) as sol = choose solutions in
        Some(sol)
    in
    chosen

  let case_c graph matching tree =
    let even_v = NTree.even_vertices tree in
    let neighboors_in_tree_edge_not_in vertex =
      VSet.diff
        (VSet.inter
           (VSet.remove vertex (VSet.of_list even_v))
           (Graph.out_neighbours vertex graph))
        (matching_neighboors_of vertex matching)
    in
    let solutions =
      List.fold_left (fun accu vertex ->
          (List.map (fun elt -> (vertex,elt))
             (VSet.elements (neighboors_in_tree_edge_not_in vertex)))@accu)
        [] even_v
    in
    let choose = List.hd in
    let chosen =
      if solutions = [] then None
      else
        let sol = choose solutions in
        Some(sol)
    in
    chosen
end

module BlossomAlgo = struct
  module VSet = Graph.VSet
  module ESet = Graph.ESet
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

  let contract_matching meta_vertex blossom matching =
    let update_vertex v = if List.mem v blossom then meta_vertex else v in
    let updated_matching =
      ESet.fold (fun (x, y) accu -> ESet.add (update_vertex x, update_vertex y) accu)
      matching ESet.empty
    in
    ESet.filter (fun (x, y) -> x <> y) updated_matching

  let add_path_to_matching matching tree last =
    ESet.union
      (ESet.of_list (NTree.even_arcs_to last tree))
      (ESet.diff
         matching
         (ESet.of_list (unoriented_arcs (NTree.uneven_arcs_to last tree))))


  let init_node graph matching =
    let solutions = unsaturated_vertices graph matching in
    let chosen =
      VSet.choose solutions
    in
    NTree.Node(chosen, [])

  (* Actions for cases *)

  open JsEmulation.Computation
  open JsEmulation.Computation.Infix

  let rec test_case_a strct =
    match Cases.case_a strct.graph strct.matching strct.tree with
    | Some (edge)->
      observe (JsContext.msg "On augmente l'arbre")
      >> observe (JsContext.end_line)
      >> observe (show_blossom_context {strct with tree = NTree.add edge strct.tree})
      >> observe (JsContext.msg "On ajoute l'arbre au couplage")
      >> observe (JsContext.end_line)
      >> return {strct with
       matching = add_path_to_matching strct.matching
           (NTree.add edge strct.tree) (snd edge);
       tree = NTree.empty}
    | None ->
      test_case_b strct

  and test_case_b strct =
    match Cases.case_b strct.graph strct.matching strct.tree with
    | Some (x, y, z)->
      let up_strct =
        {strct with
         tree = NTree.add (y, z) (NTree.add (x, y) strct.tree) }
      in
      observe (JsContext.msg "On augmente l'arbre")
      >> observe (JsContext.end_line)
      >> observe (show_blossom_context up_strct)
      >> test_case_a up_strct
    | None ->
      test_case_c strct

  and update_matching meta_vertex blossom strct =
      let update_arc (x,y) =
        let a y =
          (VSet.inter
             (ESet.fold
                (fun arc accu -> VSet.add (fst arc) accu)
                (Graph.delta_in y strct.graph) VSet.empty)
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
      let updated_matching =
        ESet.fold (fun arc accu -> ESet.add (update_arc arc) accu)
          strct.matching ESet.empty
      in
      blossom_algorithm {strct with matching = updated_matching}
      >>= fun new_strct
      -> return {new_strct with tree = NTree.empty}
        (* new_strct.graph is the contracted graph *)

  and test_case_c strct =
    match Cases.case_c strct.graph strct.matching strct.tree with
    | Some (edge)->
      let meta_vertex = find_new_vertex_name strct.graph in
      let blossom = find_blossom edge strct.tree in
      let contracted_graph = contract_graph blossom strct.graph meta_vertex in
      let contracted_tree = contract_tree meta_vertex blossom strct.tree in
      let contracted_matching = contract_matching meta_vertex blossom strct.matching in
      observe (JsContext.msg "On a trouvé une inflorescence")
      >> observe (JsContext.end_line)
      >> observe (show_blossom_context {strct with blossom = blossom})
      >> observe (JsContext.increment)
      >> observe (JsContext.msg "la on va in-depth pour trouver le couplage sans l'inflorescence il faudrais afficher plus de truc pour etre plus précis")
      >> observe (JsContext.end_line)
      >> test_case_a {strct with
                   graph = contracted_graph;
                   matching = contracted_matching;
                   tree = contracted_tree;}
      >>= fun newnn
      -> observe (JsContext.decrement)
      >> update_matching
        meta_vertex
        blossom
        {strct with matching = newnn.matching; tree = NTree.empty;}
    | None ->
      test_case_d strct

  and test_case_d strct =
    observe (JsContext.msg "On ne peut trouver de couplage dans cet arbre, on l'enlève du graphe (Il faudrait rappeller test_case_a et update_couplage comme après un blossom)")
    >> observe (JsContext.end_line)
    >> return {strct with
      graph = remove_tree_from_graph strct.graph strct.tree;
      tree = NTree.empty;}

  (* Blossom algorithm *)

  and blossom_algorithm strct =
    let nb_insature =
      VSet.cardinal (unsaturated_vertices strct.graph strct.matching) in
    if nb_insature >= 2 then
      begin
        let up_strct =
          {strct with
           tree = (init_node strct.graph strct.matching)}
        in
        observe (JsContext.msg "Initialisation de l'arbre")
        >> observe (JsContext.end_line)
        >> observe (show_blossom_context up_strct)
        >> test_case_a up_strct
        >>= fun res
        -> observe (show_blossom_context res)
        >> blossom_algorithm res
      end
    else
      begin
        return {strct with tree = NTree.empty}
      end

  (* Initialise l'algorithme *)
  let init_algorithm layout graph =
    let non_oriented_graph =
      Graph.fold_arcs
        ~f:(fun (src, dst) accu -> Graph.add_edge src dst accu)
        graph
        Graph.empty
    in
    Random.self_init ();
    { Self.empty_strct with
      layout = layout;
      graph = non_oriented_graph;
    }

  let do_blossom layout graph =
    let open JsEmulation.Computation in
    let init = init_algorithm layout graph in
    blossom_algorithm init
end



let blossom graph layout =
  let open JsEmulation.Computation in
  let open JsEmulation.Computation.Infix in
  observe (JsContext.msg "Initialisation...")
  >> observe (JsContext.end_line)
  >> BlossomAlgo.do_blossom layout graph
  >>= fun strct
  -> return strct.graph

(* Graph with a blossom *)
let graph_list3 = [(1,2);(1,4);
                   (2,1);(2,3);(2,5);
                   (3,2);(3,5);
                   (4,1);
                   (5,2);(5,3);(5,6);
                   (6,5)]
