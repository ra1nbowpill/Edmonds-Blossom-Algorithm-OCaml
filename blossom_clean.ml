module MorePerv = struct
  let cons elt list = elt :: list
  let rec repeat ~f n init =
    if n < 0 then init
    else repeat ~f (n-1) (f init)
  let compose f g x = f (g x)
  type 'value comparison = 'value -> 'value -> int
  let compose_compare compare1 compare2 (x1,y1) (x2,y2) =
    let delta = compare1 x1 x2 in
    if delta <> 0 then delta
    else compare2 y1 y2
end

module MoreModules = struct
  module type PrintableType = sig
    type t
    val to_string : t -> string
  end
  module type OrderedType = sig
    type t
    val compare : t -> t -> int
  end
  module type OrderedAndPrintableType = sig
    type t
    include PrintableType with type t := t
    include OrderedType with type t := t
  end
end

module MoreList = struct
  let rec find ~predicate = function
    | head::tail when predicate head -> Some head
    | _::tail -> find predicate tail
    | [] -> None
  let remove ~predicate list =
    let rec loop accu = function
      | head::tail when predicate head -> List.rev_append accu tail
      | head::tail -> loop (head::accu) tail
      | [] -> List.rev accu
    in loop [] list
  let rev_map ~f list =
    let rec loop accu = function
      | head::tail -> loop (f head :: accu) tail
      | [] -> accu
    in
    loop [] list
  let map ~f list = list |> rev_map ~f |> List.rev
  let rev_flatten list =
    let rec loop accu = function
      | (head::tail)::others -> loop (head::accu) (tail::others)
      | []::tail -> loop accu tail
      | [] -> accu
    in loop [] list
  let flatten list = list |> rev_flatten |> List.rev
  let bind list f = list |> List.map f |> flatten
  let rev_filter ~predicate list =
    let rec loop accu = function
      | head::tail when predicate head -> loop (head::accu) tail
      | _::tail -> loop accu tail
      | [] -> accu
    in loop [] list
  let filter ~predicate list = list |> rev_filter ~predicate |> List.rev
  let rev_partition ~predicate list =
    let rec loop accu_true accu_false = function
      | head::tail when predicate head ->
        loop (head::accu_true) accu_false tail
      | head::tail ->
        loop accu_true (head::accu_false) tail
      | [] -> (accu_true, accu_false)
    in loop [] [] list
  let partition ~predicate list =
    let (yes_items,no_items) = rev_partition ~predicate list in
    (List.rev yes_items, List.rev no_items)
  let map2
      ?(on_left_remains = fun any -> [])
      ?(on_right_remains = fun any -> [])
      ~f
    =
    let rec loop accu left right =
      match left, right with
      | [], [] -> List.rev accu
      | [], _ -> List.rev_append accu (on_right_remains right)
      | _, [] -> List.rev_append accu (on_left_remains left)
      | l1::ltail, r1::rtail ->
        loop (f l1 r1 :: accu) ltail rtail
    in
    fun left right -> loop [] left right
  let rev_take ~n list =
    let rec loop accu current_pos = function
      | _ when current_pos >= n -> accu
      | head::tail -> loop (head::accu) (current_pos+1) tail
      | [] -> accu
    in loop [] 0 list
  let take ~n list = list |> rev_take ~n |> List.rev
  let rec drop ~n = function
    | list when n <= 0 -> list
    | head::tail -> drop ~n:(n-1) tail
    | [] -> []
  let cut ~n list =
    let rec loop rev_prefix current_pos = function
      | suffix when current_pos >= n -> (rev_prefix,suffix)
      | head::tail -> loop (head::rev_prefix) (current_pos+1) tail
      | [] -> (rev_prefix,[])
    in loop [] 0 list
  let insert_nth ~n elt list =
    let rev_prefix,suffix = cut ~n list in
    List.rev_append rev_prefix (elt :: suffix)
  let remove_nth ~n list =
    let (rev_prefix,suffix) = cut ~n list in
    match suffix with
    | nth::tail -> List.rev_append rev_prefix tail
    | [] -> List.rev rev_prefix
  let range a b =
    let rec loop accu current_index =
      if current_index < a then accu
      else loop (current_index :: accu) (current_index - 1)
    in loop [] b
  let index list =
    let add_item (index,previous_items) item =
      (index+1, (index,item)::previous_items)
    in
    let (length, reversed_indexed) =
      List.fold_left add_item (0,[]) list
    in
    List.rev reversed_indexed
  let find_minimum ~objective = function
    | [] -> None
    | head::tail ->
      let min ((elt1,value1) as arg1) ((elt2,value2) as arg2) =
        if value1 < value2 then arg1 else arg2
      in
      tail
      |> List.map (fun elt -> (elt,objective elt))
      |> List.fold_left min (head,objective head)
      |> fst
      |> fun best -> Some best
  let random_predicate item = Random.bool ()
  let rec shuffle = function
    | [] -> []
    | [single] -> [single]
    | list ->
      let (left,right) = rev_partition ~predicate:random_predicate list in
      List.rev_append (shuffle left) (shuffle right)
  module Monadic = struct
    let (>>=) = bind
    let return elt = [elt]
  end
  module MakeComparableList =
    functor (Ord : MoreModules.OrderedType) ->
    struct
      type t = Ord.t list
      let rec compare list1 list2 =
        match list1, list2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | head1::tail1, head2::tail2 ->
          MorePerv.compose_compare Ord.compare compare (head1,tail1) (head2,tail2)
    end
end

module MoreOption = struct
  let bind option fct = match option with
    | None -> None
    | Some elt -> fct elt
  let default def_value = function
    | None -> def_value
    | Some thing -> thing
  let map f = function
    | None -> None
    | Some thing -> Some (f thing)
  let guard predicate = function
    | None -> None
    | Some wrong when predicate wrong -> None
    | ok -> ok
  let do_if_defined option fct = match option with
    | Some thing -> fct thing
    | None -> ()
  let of_option_list list =
    if List.for_all (fun elt -> elt <> None) list then
      Some (MoreList.bind list (function None -> [] | Some e -> [e]))
    else None
  let product opt1 opt2 = match opt1, opt2 with
    | Some elt1, Some elt2 -> Some (elt1, elt2)
    | _ -> None
  let flatten = function
    | Some e -> e
    | None -> None
  module Infix = struct
    let (>>=) = bind
  end
end

module MoreMap = struct
  module type S =
  sig
    include Map.S
    val unsafe_find : key -> 'assoc t -> 'assoc
    val find : key -> 'assoc t -> 'assoc option
  end
  module Make = functor (Key : MoreModules.OrderedType) ->
  struct
    include Map.Make(Key)
    let unsafe_find = find
    let find key map =
      if mem key map then Some (unsafe_find key map)
      else None
  end
end

module BinTree = struct
  type 'elt bintree =
    | Leaf
    | Node of 'elt bintree * 'elt * 'elt bintree
  let empty = Leaf
  let is_empty tree = tree = Leaf
  let singleton elt = Node (Leaf, elt, Leaf)
  let catamorphism ~on_leaf ~on_node tree =
    let rec loop tree k = match tree with
      | Leaf -> k on_leaf
      | Node (left,root,right) ->
        loop left @@ fun lres ->
        loop right @@ fun rres ->
        k (on_node lres (left,root,right) rres)
    in loop tree (fun id -> id)
  let height tree =
    catamorphism ~on_leaf:(-1) ~on_node:(fun l _ r -> 1 + max l r) tree
  let nbr_of_nodes tree =
    catamorphism ~on_leaf:0 ~on_node:(fun l _ r -> 1 + l + r) tree
  let rec map ~f tree =
    catamorphism
      ~on_leaf:Leaf
      ~on_node: (fun l (_,root,_) r ->  Node (l, f root, r))
      tree
  let infix_order tree =
    let rec loop tree accu = match tree with
      | Leaf -> accu
      | Node (left,root,right) ->
        accu
        |> loop left
        |> (fun l -> root::l)
        |> loop right
    in List.rev (loop tree [])
  let prefix_order tree =
    let rec loop tree accu = match tree with
      | Leaf -> accu
      | Node (left,root,right) ->
        root::accu
        |> loop left
        |> loop right
    in List.rev (loop tree [])
  let list_of_edges tree =
    let rec loop father tree accu = match tree with
      | Leaf -> accu
      | Node (left,node,right) ->
        (father,node)::accu |> loop node left |> loop node right
    in
    match tree with
    | Leaf -> []
    | Node (left,node,right) ->
      [] |> loop node left |> loop node right
end

module FloydWarshall = struct
  module type Semiring = sig
    type t
    val (+) : t -> t -> t
    val ( * ) : t -> t -> t
  end
  module Make =
    functor (Ring : Semiring) ->
    functor (Elt : Set.OrderedType) ->
    functor (S : Set.S with type elt = Elt.t) ->
    struct
      open Ring
      module Pair = struct
        type t = Elt.t * Elt.t
        let compare = MorePerv.compose_compare Elt.compare Elt.compare
      end
      module PMap = MoreMap.Make(Pair)
      type closure = S.elt -> S.elt -> Ring.t option
      let link all_pairs source destination =
        PMap.find (source,destination) all_pairs
      let fold_pairs ~f set init =
        let fold fct = S.fold fct set in
        init |>
        fold @@ fun src ->
        fold @@ fun dst -> f src dst
      let map_add_if_some key assoc map =
        match assoc with
        | None -> map
        | Some assoc -> PMap.add key assoc map
      let sum_option option1 option2 =
        match option1, option2 with
        | None, _ -> option2
        | _, None -> option1
        | Some opt1, Some opt2 -> Some (opt1+opt2)
      let init_all_pairs ~d graph =
        let f src dst pmap =
          map_add_if_some (src,dst) (d src dst) pmap
        in
        fold_pairs ~f graph PMap.empty
      let fw_single_round set new_vertex all_pairs  =
        let f src dst pmap =
          let old_value = PMap.find (src,dst) pmap in
          let new_value =
            let open MoreOption.Infix in
            PMap.find (src,new_vertex) pmap >>= fun prefix ->
            PMap.find (new_vertex,dst) pmap >>= fun suffix ->
            Some (prefix * suffix)
          in
          let combined_value = sum_option old_value new_value in
          map_add_if_some (src,dst) combined_value pmap
        in
        fold_pairs ~f set all_pairs
      let transitive_closure ~d set =
        S.fold
          (fw_single_round set)
          set
          (init_all_pairs ~d set)
        |> link
    end
end

module Graph = struct
  module Vertex = struct
    type t = int
    let compare = compare
  end
  module Arc = struct
    type t = int * int
    let compare = compare
  end
  module VSet = Set.Make(Vertex)
  module VMap = MoreMap.Make(Vertex)
  module ESet = Set.Make(Arc)
  module EMap = MoreMap.Make(Arc)
  type vertex = Vertex.t
  type arc = Arc.t
  type t =
    {
      vertices : VSet.t;
      in_adjacency : ESet.t VMap.t;
      out_adjacency : ESet.t VMap.t

    }
  (* *** basic usage *** *)
  let is_empty { vertices } = VSet.is_empty vertices
  let order graph = VSet.cardinal graph.vertices
  let vertices graph = graph.vertices

  let is_vertex vertex graph =
    VSet.mem vertex graph.vertices
  let delta_in vertex graph =
    VMap.find vertex graph.in_adjacency
    |> MoreOption.default ESet.empty
  let delta_out vertex graph =
    VMap.find vertex graph.out_adjacency
    |> MoreOption.default ESet.empty
  let has_arc ~src ~dst graph =
    ESet.exists (fun (u,v) -> v = dst) (delta_out src graph)
  let are_adjacent u v graph =
    has_arc ~src:u ~dst:v graph
    || has_arc ~src:v ~dst:v graph
  let in_neighbours vertex graph =
    ESet.fold (fun (src,dst) -> VSet.add dst) (delta_out vertex graph) VSet.empty
  let out_neighbours vertex graph =
    ESet.fold (fun (src,dst) -> VSet.add src) (delta_in vertex graph) VSet.empty
  let fold_vertices ~f graph init =
    VSet.fold f graph.vertices init
  let fold_arcs ~f:fct graph init =
    VMap.fold
      (fun src -> ESet.fold fct)
      graph.out_adjacency
      init

  (* *** building a graph *** *)
  let empty =
    {
      vertices = VSet.empty;
      in_adjacency = VMap.empty;
      out_adjacency = VMap.empty
    }
  let add_vertex vertex graph =
    if is_vertex vertex graph then graph
    else
      {
        vertices = VSet.add vertex graph.vertices;
        in_adjacency = VMap.add vertex ESet.empty graph.in_adjacency;
        out_adjacency = VMap.add vertex ESet.empty graph.out_adjacency

      }
  let add_adjacency ~src ~dst graph =
    let out_src = delta_out src graph in
    let in_dst = delta_in dst graph in
    { graph with
      in_adjacency =
        VMap.add dst (ESet.add (src,dst) in_dst) graph.in_adjacency;
      out_adjacency =
        VMap.add src (ESet.add (src,dst) out_src) graph.out_adjacency;
    }
  let add_arc ~src ~dst graph =
    if has_arc ~src ~dst graph then graph
    else
      graph
      |> add_vertex src
      |> add_vertex dst
      |> add_adjacency ~src ~dst
  let add_edge source destination graph =
    graph
    |> add_arc ~src:source ~dst:destination
    |> add_arc ~src:destination ~dst:source
  let remove_arc ~src ~dst graph =
    if not (has_arc ~src ~dst graph) then graph
    else
      let in_src = delta_in dst graph in
      let out_src = delta_out src graph in
      { graph with
        in_adjacency =
          VMap.add dst (ESet.remove (src,dst) in_src) graph.in_adjacency;
        out_adjacency =
          VMap.add src (ESet.remove (src,dst) out_src) graph.out_adjacency;
      }
  let remove_edge source destination graph =
    graph
    |> remove_arc ~src:source ~dst:destination
    |> remove_arc ~src:destination ~dst:source
  let remove_vertex vertex graph =
    if not (is_vertex vertex graph) then graph
    else
      let remove_arc (src,dst) = remove_arc ~src ~dst in
      let arcs = ESet.union (delta_in vertex graph) (delta_out vertex graph) in
      let new_graph = ESet.fold remove_arc (arcs) graph in
      {
        vertices = VSet.remove vertex new_graph.vertices;
        in_adjacency = VMap.remove vertex new_graph.in_adjacency;
        out_adjacency = VMap.remove vertex new_graph.out_adjacency;
      }

  (* *** distance-based algorithms *** *)
  module ShortPath = struct
    type t = int * vertex option BinTree.bintree
    let (+) (len1,path1) (len2,path2) =
      if len1 <= len2 then (len1,path1)
      else (len2,path2)
    and ( * ) (len1,path1) (len2,path2) =
      (len1 + len2, BinTree.(Node (path1, None, path2)))
  end
  module FW = FloydWarshall.Make(ShortPath)(Vertex)(VSet)
  type all_shortest_paths = FW.closure
  let all_pairs_shortest_paths ~d graph =
    let d src dst = match d src dst with
      | None -> None
      | Some length ->
        Some (length, BinTree.singleton (Some dst))
    in
    FW.transitive_closure ~d graph.vertices
  let distance closure ~src ~dst =
    match closure src dst with
    | None -> None
    | Some (dist,bintree) -> Some dist
  let shortest_path closure ~src ~dst =
    match closure ~src ~dst with
    | None -> None
    | Some (len,tree) ->
      let open MoreList.Monadic in
      let vertices =
        BinTree.infix_order tree >>= function
        | None -> []
        | Some e -> [e]
      in
      Some (src::vertices)
end























let rec print_list f = function
  | [] -> ()
  | [elt] ->
    f elt;
  | head::tail ->
    f head;
    print_list f tail

let print_couple_int_list =  print_list (fun (x, y) -> Printf.printf "(%d,%d)" x y)

let print_int_list = print_list (fun x -> Printf.printf "%d" x)

let print_vertex_list = print_int_list
let print_arcs_list = print_list (fun (x, y) -> Printf.printf "(%d,%d)" x y)




module VSet = Graph.VSet
module ESet = Graph.ESet

type tree =
  | Node of (Graph.vertex * tree list)

let print_eset eset =
  print_arcs_list (ESet.elements eset)
let print_vset vset =
  print_vertex_list (VSet.elements vset)

let print_delta str f graph =
  VSet.fold
    (fun vertex _ ->
       Printf.printf "%s[%d] : " str vertex;
       print_eset (f vertex graph);
       Printf.printf "\n")
    (Graph.vertices graph)
    ()

let print_delta_in =
  print_delta "in" Graph.delta_in

let print_delta_out =
  print_delta "out" Graph.delta_out

let rec tree_add (x, y) tree =
  match tree with
  | Node(v, []) -> if v == x then Node(v, [Node(y, [])]) else Node(v, [])
  | Node(v, childs) ->
    if v == x then Node(v, (Node(y, []))::childs)
    else Node(v, List.map (fun node -> tree_add (x, y) node) childs)

let rec get_node tree vertex =
  match tree with
  | Node(v, []) -> if v == vertex then Some (Node(v, [])) else None
  | Node(v, childs) ->
    if v == vertex then
      Some (Node(v, childs))
    else
      match get_node (List.hd childs) vertex with
      | Some (x) -> Some(x)
      | None ->
        match get_node (Node(v, List.tl childs)) vertex with
        | Some (y) -> Some(y)
        | None -> None

let rec get_vertices tree parite =
  match tree with
  | Node(v, []) ->
    if parite then [v] else []
  | Node(v, childs) ->
    let res =
      List.fold_left (fun accu node -> (get_vertices node (not parite))@accu) [] childs
    in
    if parite then
      v::res
    else
      res

let even_vertices tree =
  get_vertices tree true

let uneven_vertices tree =
  get_vertices tree false

let merge_couple ~f (even1, uneven1) (even2, uneven2) =
  (f even1 even2, f uneven1 uneven2)

let list_un_even_edges_of_tree tree =
  let rec my_fun tree accu parite =
    match tree with
    | Node (v, []) -> accu
    | Node (v1, l::ist) ->
      let Node (v2, _) = l in
      if parite then
        merge_couple ~f:List.append
          (my_fun l ([(v1,v2)], []) (not parite))
          (my_fun (Node(v1, ist)) accu parite)
      else
       merge_couple ~f:List.append
          (my_fun l accu (not parite))
          (my_fun (Node(v1, ist)) ([], [(v1,v2)]) parite)
  in
  my_fun tree ([], []) true


let un_even_edges_of_tree tree =
  let rec my_fun tree accu parite =
    match tree with
    | Node (_, []) -> accu
    | Node (v1, l::ist) ->
      let Node (v2, _) = l in
      if parite then
        merge_couple ~f:ESet.union
          (my_fun l (ESet.add (v1,v2) ESet.empty, ESet.empty) (not parite))
          (my_fun (Node(v1, ist)) accu parite)
      else
        merge_couple ~f:ESet.union
          (my_fun l (ESet.empty, ESet.add (v1,v2) ESet.empty) (not parite))
          (my_fun (Node(v1, ist)) accu parite)
  in
  my_fun tree (ESet.empty, ESet.empty) true

let un_even_path_of_tree tree last =
  let rec my_fun tree accu parite =
    match tree with
    | Node (v, []) -> if last ==v then accu else (ESet.empty, ESet.empty)
    | Node (v1, l::ist) ->
      let Node (v2, _) = l in
      if parite then
        merge_couple ~f:ESet.union
          (my_fun l (ESet.add (v1,v2) (fst accu), snd accu) (not parite))
          (my_fun (Node(v1, ist)) accu parite)
      else
        merge_couple ~f:ESet.union
          (my_fun l (fst accu, ESet.add (v1,v2) (snd accu)) (not parite))
          (my_fun (Node(v1, ist)) accu parite)
  in
  my_fun tree (ESet.empty, ESet.empty) true

let rec path_of_tree tree vertex =
  match tree with
  | Node (v, []) -> if vertex == v then Some([v]) else None
  | Node (v, childs) ->
    match path_of_tree (List.hd childs) vertex with
    | Some (x) -> Some(v::x)
    | None ->
      match path_of_tree (Node(v, List.tl childs)) vertex with
      | Some (y) -> Some(v::y)
      | None -> None

let rec get_node tree vertex =
  match tree with
  | Node(v, []) -> if v == vertex then Some (Node(v, [])) else None
  | Node(v, childs) ->
    if v == vertex then
      Some (Node(v, childs))
    else
      match get_node (List.hd childs) vertex with
      | Some (x) -> Some(x)
      | None ->
        match get_node (Node(v, List.tl childs)) vertex with
        | Some (y) -> Some(y)
        | None -> None

let path_of_tree_from_to tree src dst =
  match get_node tree src with
  | None -> None
  | Some(node) ->
    match path_of_tree node dst with
    | None -> None
    | Some(lst) -> Some(lst)

let rec eset_of_tree tree =
  match tree with
  | Node(v, []) -> ESet.empty
  | Node(v1, Node(v2, lst)::next) ->
    ESet.union
      (ESet.add (v1, v2) (eset_of_tree (Node(v2, lst))))
      (eset_of_tree (Node(v1, next)))

let rec vset_of_tree tree =
  match tree with
  | Node(v, []) -> VSet.add v VSet.empty
  | Node(v, node::next) ->
    VSet.union
      (VSet.add v (vset_of_tree node))
      (vset_of_tree (Node(v, next)))

let rec vertices_of_tree tree =
  match tree with
  | Node(v, []) -> [v]
  | Node(v, node::next) ->
    (v::(vertices_of_tree node))@
    (vertices_of_tree (Node(v, next)))

let rec vset_of_eset edges =
  ESet.fold
    (fun (x, y) accu -> VSet.add x (VSet.add y accu))
    edges
    VSet.empty

let vset_of_list vertices =
  List.fold_right VSet.add vertices VSet.empty
let eset_of_list edges =
  List.fold_right ESet.add edges ESet.empty
let graph_of_list edges =
  List.fold_right (fun (x, y) accu -> Graph.add_edge x y accu) edges Graph.empty
let tree_of_list edges =
  List.fold_left (fun tree edge -> tree_add edge tree) (Node(fst (List.hd edges), [])) edges

let remove_vertices_from_graph graph vertices =
  List.fold_left
    (fun accu vertex -> Graph.remove_vertex vertex accu)
    graph
    vertices

let remove_vset_from_graph graph vset =
  let vertices = VSet.elements vset in
  remove_vertices_from_graph graph vertices

let remove_tree_from_graph graph tree =
  let vertices = vertices_of_tree tree in
  remove_vertices_from_graph graph vertices

let saturated_vertices couplage =
  ESet.fold (fun (x, y) accu -> VSet.add x (VSet.add y accu))
    couplage
    VSet.empty

let unsaturated_vertices graph couplage =
  VSet.diff (Graph.vertices graph) (saturated_vertices couplage)

let neighboors_of vertex graph =
  Graph.out_neighbours vertex graph

let couplage_neighboors_of vertex couplage =
  ESet.fold
    (fun (x, y) accu ->
       if x == vertex then VSet.add y accu
       else if y == vertex then VSet.add x accu
       else accu)
    couplage
    VSet.empty

let case_a graph couplage tree =
  let even_v = even_vertices tree in
  let rec my_fun list_even =
    match list_even with
    | [] -> None
    | x::next ->
      let solutions =
        VSet.diff
          (neighboors_of x graph)
          (VSet.union
             (saturated_vertices couplage)
             (vset_of_tree tree))
      in
      if VSet.is_empty solutions then
        my_fun next
      else
        let sol = VSet.choose solutions in
        Some(x, sol)
  in
  my_fun even_v

let case_b graph couplage tree =
  let even_v = even_vertices tree in
  let rec my_fun2 ys =
    match ys with
    | [] -> None
    | y::next ->
      let zs = VSet.diff
          (VSet.inter
             (neighboors_of y graph)
             (couplage_neighboors_of y couplage))
          (VSet.add y (vset_of_tree tree))
      in
      if VSet.is_empty zs then
        my_fun2 next
      else
        Some(y, VSet.choose zs)
  in
  let rec my_fun list_even =
    match list_even with
    | [] -> None
    | x::next ->
      let ys = VSet.diff
          (neighboors_of x graph)
          (VSet.union
             (vset_of_tree tree)
             (couplage_neighboors_of x couplage))
      in
      if VSet.is_empty ys then
        my_fun next
      else
        match my_fun2 (VSet.elements ys) with
        | None -> my_fun next
        | Some(y, z) -> Some(x, y, z)
  in
  my_fun even_v

let case_c graph couplage tree =
  let even_v = even_vertices tree in
  let rec my_fun list_even =
    match list_even with
    | [] -> None
    | p1::next ->
      let p2 = VSet.diff
          (VSet.inter
             (VSet.remove p1 (VSet.of_list even_v))
             (neighboors_of p1 graph))
          (couplage_neighboors_of p1 couplage)
      in
      if VSet.is_empty p2 then
        my_fun next
      else
        Some(p1, VSet.choose p2)
  in
  my_fun even_v


let find_blossom edge tree =
  match path_of_tree_from_to tree (fst edge) (snd edge) with
  | Some (lst) -> lst
  | None ->
    match path_of_tree_from_to tree (snd edge) (fst edge) with
    | Some (lst) -> lst
    | None -> []

let find_new_vertex_name graph =
  (Graph.fold_vertices ~f:(fun x y -> if (x > y) then x else y) graph (-999999)) + 1

let contract_blossom_in_graph blossom graph =
  let extract f =
    List.fold_left
      (fun accu vertex -> ESet.union (f vertex graph) accu)
      ESet.empty
      (blossom)
  in
  let vall x blossom meta = if List.mem x blossom then meta else x in
  let update_arcs arcs blossom meta_vertex =
    ESet.fold
      (fun (x, y) accu ->
         ESet.add
           (vall x blossom meta_vertex,
           vall y blossom meta_vertex)
          accu)
      arcs
      ESet.empty
  in
  let meta_vertex = find_new_vertex_name graph in
  let meta_in_arcs =
    Printf.printf "New vertex : %d\n" meta_vertex;
    update_arcs (extract Graph.delta_in) blossom meta_vertex in
  let meta_out_arcs =
    Printf.printf "meta_in_arcs : ";
    print_eset meta_in_arcs; Printf.printf "\n" ;
    update_arcs (extract Graph.delta_out) blossom meta_vertex in
  let arcs =
    Printf.printf "meta_out_arcs : ";
    print_eset meta_out_arcs ; Printf.printf "\n";
    ESet.union meta_out_arcs meta_in_arcs in
  let cleaned_graph =
    remove_vertices_from_graph
      (Graph.add_vertex
         meta_vertex
         graph)
      blossom
  in
  print_delta_in graph;
  Printf.printf "\n";
  print_delta_in cleaned_graph;
  Printf.printf "\n";
  print_delta_out graph;
  Printf.printf "\n";
  print_delta_out cleaned_graph;
  Printf.printf "\nBlossom : ";
  print_vertex_list blossom;
  Printf.printf "\narcs : ";
  print_eset arcs;
  Printf.printf "\narcs : ";
  print_eset (ESet.remove (meta_vertex, meta_vertex) arcs);
  Printf.printf "\n";
  ESet.fold
    (fun (src, dst) accu ->
       (* Est-ce quon peut avoir un graphe orienté ?????? add_arc ou add_edge???? *)
       Graph.add_edge src dst  accu)
    (ESet.remove (meta_vertex, meta_vertex) arcs)
    cleaned_graph

let rec remove_blossom_from_tree (x, y) tree =
  let contract_blossom x y tree =
    match get_node tree x with
    | Some(path) -> path
    | None ->
      Printf.printf "remove_blossom_from_tree error : path from %d to %d do not exist in the tree.\n" x y;
      Node(0, [])
  in
  match tree with
  | Node(v, []) ->
    if (v == x || v == y) then
      Printf.printf "remove_blossom_from_tree error : blossom begins at a leaf of the tree.\n"
    else
      Printf.printf "" ;
    Node(v, [])
  | Node(v, childs) ->
    if v == x  then
      contract_blossom x y tree
    else if v == y  then
      contract_blossom y x tree
    else
      Node(v, List.map (fun node ->  remove_blossom_from_tree (x, y) node) childs)

let add_path_to_couplage couplage tree last =
  let paths = un_even_path_of_tree tree last in
  ESet.union
    (fst paths)
    (ESet.diff
       couplage
       (snd paths))

let rec test_case_a graph couplage tree =
  Printf.printf "Cas A : ";
  match case_a graph couplage tree with
  | Some (edge)->
    let path =  un_even_path_of_tree (tree_add edge tree) (snd edge) in
    Printf.printf "√ : ";
    print_couple_int_list (ESet.elements (fst path));
    print_couple_int_list (ESet.elements (snd path));
    Printf.printf "\n";
    (graph, add_path_to_couplage couplage (tree_add edge tree) (snd edge))
  | None ->
    Printf.printf "X\n";
    test_case_b graph couplage tree

and test_case_b graph couplage tree =
  Printf.printf "Cas B : ";
  match case_b graph couplage tree with
  | Some (x, y, z)->
    Printf.printf "√ : Ajout de (%d, %d) (%d, %d) au chemin\n" x y y z;
    test_case_a
      graph
      couplage
      (tree_add (y, z) (tree_add (x, y) tree))
  | None ->
    Printf.printf "X\n";
    test_case_c graph couplage tree

and test_case_c graph couplage tree =
  Printf.printf "Cas C : ";
  match case_c graph couplage tree with
  | Some (edge)->
    Printf.printf "√\n";
    let blossom = find_blossom edge tree in
    Printf.printf "Un cycle a été trouvé : ";
    print_int_list blossom;
    Printf.printf "\n";
    test_case_a
      (contract_blossom_in_graph blossom graph)
      couplage
      (remove_blossom_from_tree edge tree)
  | None ->
    Printf.printf "X\n";
    test_case_d graph couplage tree

and test_case_d graph couplage tree =
  Printf.printf "Cas D\n";
  (remove_tree_from_graph graph tree, couplage)

let init_node graph couplage =
  Node(VSet.choose (unsaturated_vertices graph couplage), [])

let rec blossom_algorithm x (graph, couplage) =
  Printf.printf "\n%d\n" x;
  if x == 0 then
    (graph, couplage)
  else
    let nb_insature = VSet.cardinal (unsaturated_vertices graph couplage) in
    Printf.printf "Couplage actuel : ";
    if ESet.is_empty couplage then
      Printf.printf "[]"
    else
      print_couple_int_list (ESet.elements couplage);
  Printf.printf "\n";
    Printf.printf "Nombre de sommets insaturés : %d\n" nb_insature ;
    if nb_insature >= 1 then
      begin
      Printf.printf "On construit un arbre\n" ;
      test_case_a graph couplage (init_node graph couplage)
      |> blossom_algorithm (x - 1)
      end
  else
    begin
      Printf.printf "Plus assez de sommets insaturés\n";
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
  |> blossom_algorithm 10


let graph_list = [(1,8);(1,2);(1,5);
                  (2,1);(2,8);(2,3);
                  (3,5);(3,10);(3,2);(3,9);(3,6);
                  (4,5);(4,7);(4,6);
                  (5,1);(5,3);(5,4);(5,7);
                  (6,4);(6,7);(6,9);(6,3);
                  (7,5);(7,4);(7,6);
                  (8,1);(8,2);
                  (9,3);(9,6);
                  (10,3)]
let couplage_list = [(1,8);(2,3);(6,9)]
let tree_list = [(5,7);(7,4);(5,6);(7,8)]

let _ = tree_add (2, 4) (tree_add (2,3) (tree_add (1, 2) (Node(1,[]))))

let graph = graph_of_list graph_list
let couplage = eset_of_list couplage_list
let tree = tree_of_list tree_list

let _ = VSet.elements (saturated_vertices couplage)             (* 1,2,3,6,8,9 *)
let _ = VSet.elements (unsaturated_vertices graph couplage)     (* 4,5,7,10 *)
let _ = ESet.elements (eset_of_tree tree)                       (* [(5,7);(7,4)] *)
let _ = VSet.elements (vset_of_tree tree)                       (* 5,7,4 *)
let _ = even_vertices tree                      (* 5,8,4 *)
let _ = uneven_vertices tree                    (* 6,7 *)

let _ = un_even_edges_of_tree tree
let _ = (ESet.elements (fst (un_even_path_of_tree tree 8)))
let _ = (ESet.elements (snd (un_even_path_of_tree tree 8)))

(*let _ = do_blossom graph*)

let (g, c) = init_algorithm graph
let a = init_node g c
let _ = VSet.cardinal (unsaturated_vertices g c)
let b = test_case_a g c a


let graph = graph_of_list graph_list
let tree_list = [(8,1);(1,2)]
let tree = tree_of_list tree_list
let couplage = ESet.empty

let rec remove_blossom_from_tree (x, y) tree =
  Printf.printf "edge : (%d, %d)\n" x y;
  let contract_blossom x y tree =
    match get_node tree x with
    | Some(path) -> path
    | None ->
      Printf.printf "remove_blossom_from_tree error : path\
                     from %d to %d do not exist in the tree.\n" x y;
      Node(0, [])
  in
  match tree with
  | Node(v, []) ->
    Printf.printf "sommet : %d\n" v;
    if (v == x || v == y) then
      Printf.printf "remove_blossom_from_tree error : blossom begins at a leaf of the tree.\n"
    else
      Printf.printf "" ;
    Node(v, [])
  | Node(v, childs) ->
    Printf.printf "sommet : %d\n" v;
    if v == x  then
      contract_blossom x y tree
    else if v == y  then
      contract_blossom y x tree
    else
      Node(v, List.map (fun node ->  remove_blossom_from_tree (x, y) node) childs)

let blossom_edge = match  case_c graph couplage tree with | Some(edge) -> edge | None -> (0,0)
let my_blossom = find_blossom blossom_edge tree
let new_tree = remove_blossom_from_tree blossom_edge tree
let new_graph = contract_blossom_in_graph my_blossom graph

let x =
  Graph.empty
  |> Graph.add_arc ~src:1 ~dst:2
  |> Graph.add_arc ~src:2 ~dst:1
  |> Graph.remove_vertex 2
let _ = print_delta_in x
let _ = print_delta_out x

let _ = get_node tree 2
(* TODO : Revoir remove_blossom_from_tree *)

(*
let edges = [(0,6); (0,7); (6,7); (6,5); (2,7); (5,7); (5,2); (5,1); (1,2); (2,3); (2,4); (3,4)]
let graph = List.fold_right (fun (src, dst) -> Graph.add_edge src dst) (edges) (Graph.empty)
let tree = build_tree graph
*)
