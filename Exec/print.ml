let rec print_tree =  function NTree.Node(v, childs) ->
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
