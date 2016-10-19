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

module Tree = struct

  (* Arbitrary tree *)

  type value = Graph.vertex
  type t = Node of (value * t list)

  (* Getters *)

  let value = function Node(v, _) -> v

  let childs = function Node(_, childs) -> childs

  (* Tree management *)

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
      my_fun tree


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

  let path_to vertex =
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

  let path_from_to src dst tree =
    match find src tree with
    | None -> None
    | Some(node) -> let a = path_to dst node in
      if a = [] then None else Some(path_to dst node)

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
        (elt2, elt1)::p_arcs_of_path (not parite) (elt2::next)
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

  let create_node v lst = Node(v, [])
  let of_arcs lst =
    List.fold_left (fun tree arc -> add arc tree) (create_node (fst (List.hd lst)) []) lst
  let of_eset arcs = of_arcs (Graph.ESet.elements arcs)

end

module Print = struct
  (* *** Printing functions *** *)

  let rec print_tree =  function Tree.Node(v, childs) ->
    let rec my_fun = function
      | [] -> ()
      | elt::[] -> print_tree elt
      | head::tail -> print_tree head ; Printf.printf "," ; my_fun tail
    in
    Printf.printf "%d" v;
    if childs <> [] then Printf.printf " {";
    my_fun childs;
    if childs <> [] then Printf.printf "}"

  let rec print_list_base print_elt ?(print_last=(print_elt)) lst =
    match lst with
    | [] -> ()
    | head::tail ->
      if tail = [] then print_last head
      else
        (print_elt head;
         print_list_base print_elt tail)

  let print_couple print_fst print_snd (x, y) =
    Printf.printf "(";
    print_fst x ; Printf.printf ", " ; print_snd y ;
    Printf.printf ")"

  let print_int = Printf.printf "%d"


  let print_list_beauty print_elt lst=
    Printf.printf "[";
    print_list_base
      (fun a -> print_elt a ; Printf.printf ", ")
      ~print_last:print_elt
      lst;
    Printf.printf "]"

  let print_list = print_list_beauty

  let print_int_list = print_list print_int

  let print_vertex = print_int
  let print_vertex_list = print_list_beauty print_vertex

  let print_arc = print_couple (print_vertex) (print_vertex)
  let print_arc_list = print_list_beauty print_arc

  let print_eset eset =
    print_arc_list (Graph.ESet.elements eset)
  let print_vset vset =
    print_vertex_list (Graph.VSet.elements vset)

  let print_delta str f graph =
    Graph.VSet.fold
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

end

module Conversion = struct
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
    List.fold_left (fun tree edge -> Tree.add edge tree) (Tree.Node(fst (List.hd edges), [])) edges
end

module MoreGraph = struct
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
    let vertices = Tree.vertices tree in
    remove_vertices_from_graph graph vertices

  (* Couplage & Graph getters *)
  let saturated_vertices couplage =
    ESet.fold (fun (x, y) accu -> VSet.add x (VSet.add y accu))
      couplage
      VSet.empty
  let unsaturated_vertices graph couplage =
    VSet.diff (Graph.vertices graph) (saturated_vertices couplage)
  let couplage_neighboors_of vertex couplage =
    ESet.fold
      (fun (x, y) accu ->
         if x == vertex then VSet.add y accu
         else if y == vertex then VSet.add x accu
         else accu)
      couplage
      VSet.empty
end

(* a, b, c, d cases test *)

module Cases = struct
  module VSet = Graph.VSet
  module ESet = Graph.ESet
  open Print
  open MoreGraph

  let case_a graph couplage tree =
    let even_v = Tree.even_vertices tree in
    let compute_solutions vertex =
      VSet.diff
        (Graph.out_neighbours vertex graph)
        (VSet.union
           (saturated_vertices couplage)
           (Tree.vset tree))
    in
    let solutions =
      List.fold_left
        (fun accu vertex ->
           ESet.union
             (Conversion.eset_of_vset vertex (compute_solutions vertex))
             accu)
        ESet.empty even_v
    in
    let chosen =
      if ESet.is_empty solutions then None
      else
        let sol = ESet.choose solutions in
        (print_arc sol; Printf.printf "\n"; Some(ESet.choose solutions))
    in
    print_eset solutions;Printf.printf "\n";
    chosen

  let case_b graph couplage tree =
    let even_v = Tree.even_vertices tree in
    let compute_ys vertex =
      VSet.diff
        (Graph.out_neighbours vertex graph)
        (VSet.union
           (Tree.vset tree)
           (couplage_neighboors_of vertex couplage))
    in
    let compute_zs vertex =
      VSet.diff
        (VSet.inter
           (Graph.out_neighbours vertex graph)
           (couplage_neighboors_of vertex couplage))
        (VSet.add vertex (Tree.vset tree))
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
    let chosen =
      if solutions = [] then None
      else
        let (x,y,z) as sol = List.hd solutions in
        Printf.printf "(%d,%d,%d)" x y z; Some(sol)
    in
    print_list (fun (x,y,z) -> Printf.printf "(%d,%d,%d)" x y z) solutions;
    Printf.printf "\n";
    chosen

  let case_c graph couplage tree =
    let even_v = Tree.even_vertices tree in
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
    let chosen =
      if solutions = [] then None
      else
        let sol = List.hd solutions in
        print_arc sol; Printf.printf "\n"; Some(sol)
    in
    print_arc_list solutions; Printf.printf "\n";
    chosen
end

module BlossomAlgo = struct
  module VSet = Graph.VSet
  module ESet = Graph.ESet
  open Print
  open Conversion
  open MoreGraph
  (* blossom management *)

  let find_blossom edge tree =
    match Tree.path_from_to (fst edge) (snd edge) tree with
    | Some (lst) -> lst
    | None ->
      match Tree.path_from_to (snd edge) (fst edge) tree with
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
    let meta_vertex = find_new_vertex_name graph in
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

  let rec remove_blossom_from_tree (x, y) tree =
    let contract_blossom x y tree =
      match Tree.find y tree with
      | Some(path) ->
        Printf.printf "Found %d\n" y; path
      | None ->
        Printf.printf "remove_blossom_from_tree error : path\
                       from %d to %d does not exist in the tree.\n" x y;
        Tree.Node(0, [])
    in
    match tree with
    | Tree.Node(v, []) as node ->
      if (v == x || v == y) then
        Printf.printf "remove_blossom_from_tree error : blossom\
                       begins at a leaf of the tree.\n";
      node
    | Tree.Node(v, childs) ->
      if v == x  then
        (Printf.printf "Found %d\n" v ;
         contract_blossom x y tree)
      else if v == y  then
        (Printf.printf "Found %d\n" v ;
         contract_blossom y x tree)
      else
        Tree.Node(v, List.map (fun node ->  remove_blossom_from_tree (x, y) node) childs)

  let anti_arc (x,y) = (y,x)
  let anti_arcs = List.map anti_arc
  let anti_arcs_eset eset = ESet.of_list (anti_arcs (ESet.elements eset))

  let unoriented_arcs_eset eset =
    ESet.union eset (anti_arcs_eset eset)
  let unoriented_arcs lst =
    ESet.elements (unoriented_arcs_eset (ESet.of_list lst))


  let add_path_to_couplage couplage tree last =
    Printf.printf "\n";
    print_arc_list (Tree.even_arcs_to last tree); Printf.printf "\n";
    print_arc_list (Tree.uneven_arcs_to last tree); Printf.printf "\n";
    ESet.union
      (ESet.of_list (Tree.even_arcs_to last tree))
      (ESet.diff
         couplage
         (ESet.of_list (unoriented_arcs (Tree.uneven_arcs_to last tree))))

  (* Actions for cases *)

  let rec test_case_a graph couplage tree =
    Printf.printf "Tree : "; print_tree tree; Printf.printf "\n";
    Printf.printf "Cas A : On cherche (x,y)
avec x pair & x et y nApp (couplage U tree)\n";
    match Cases.case_a graph couplage tree with
    | Some (edge)->
      (graph, add_path_to_couplage couplage (Tree.add edge tree) (snd edge))
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
        (Tree.add (y, z) (Tree.add (x, y) tree))
    | None ->
      test_case_c graph couplage tree

  and test_case_c graph couplage tree =
    Printf.printf "Cas C : on cherche (x, y)
avec x et y pair & (x,y) nApp tree\n";
    match Cases.case_c graph couplage tree with
    | Some (edge)->
      let blossom = find_blossom edge tree in
      let contracted_graph = contract_blossom_in_graph blossom graph in
      let contracted_tree = remove_blossom_from_tree edge tree in
      Printf.printf "blossom : "; print_int_list blossom; Printf.printf "\n";
      Print.print_delta_out graph; Printf.printf "\n";
      Print.print_delta_out contracted_graph; Printf.printf "\n";
      Print.print_tree tree; Printf.printf "\n";
      Print.print_tree contracted_tree; Printf.printf "\n";
      test_case_a contracted_graph couplage contracted_tree
    | None ->
      test_case_d graph couplage tree

  and test_case_d graph couplage tree =
    Printf.printf "Cas D\n";
    (remove_tree_from_graph graph tree, couplage)

  (* Blossom algorithm *)

  let init_node graph couplage x =
    let solutions = unsaturated_vertices graph couplage in
    let chosen =
      if x = 10 then 5
      else VSet.choose solutions
    in
    Tree.Node(chosen, [])

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
        print_eset couplage;
      Printf.printf "\n";
      Printf.printf "Nombre de sommets insaturés : %d\n" nb_insature ;
      if nb_insature >= 1 then
        begin
          Printf.printf "On construit un arbre\n" ;
          test_case_a graph couplage (init_node graph couplage x)
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
end

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

let graph = Conversion.graph_of_list graph_list
let couplage = Conversion.eset_of_list couplage_list
let tree = Conversion.tree_of_list tree_list

let _ = BlossomAlgo.do_blossom graph

let _ = ()

(*let _ = VSet.elements (saturated_vertices couplage)             (* 1,2,3,6,8,9 *)
  let _ = VSet.elements (unsaturated_vertices graph couplage)     (* 4,5,7,10 *)
  let _ = ESet.elements (Tree.eset tree)                       (* [(5,7);(7,4)] *)
  let _ = VSet.elements (Tree.vset tree)                       (* 5,7,4,6,8 *)
  let _ = Tree.even_vertices tree                      (* 5,8,4 *)
  let _ = Tree.uneven_vertices tree                    (* 6,7 *)

  let _ = Tree.even_arcs_to 8 tree
  let _ = Tree.uneven_arcs_to 8 tree*)

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
open Tree
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
