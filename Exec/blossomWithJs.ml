module GraphValue =
struct
  type t = Graph.t * (Graph.vertex -> Gg.p2)
  let to_image (graph,layout) =
    MoreImage.draw_graph ~layout graph
end

module Console =
  Console.Make
    (JsContext)
    (JsEmulation.Emulator)
    (GraphValue)

let send_undefined graph_name =
  let msg = Printf.sprintf "Graph {%s} is undefined" graph_name in
  Console.send_messages Tree.(Node (msg,[]))

let if_defined name kont =
  let open State.Infix in
  Console.find name >>= function
  | None -> send_undefined name
  | Some graph -> kont graph


let register_blossom graph_name (graph, layout) =
  if Graph.VSet.is_empty (Graph.vertices graph) then
    Console.send_messages
      Tree.(Node ("Empty graph.", []))
  else
    let computation = Blossom.blossom graph layout in
    let continuation tree =
      Console.add
        (Printf.sprintf "%s_tree" graph_name)
        (tree, layout)
    in
    Console.register computation continuation


let layout =
  List.fold_right
    (fun (v,x,y) -> Graph.VMap.add v (Gg.P2.v x y))
    []
    Graph.VMap.empty

(* EditableGraph vide *)
let init_graph =
  EditableGraph.create
    ~graph:Graph.empty
    ~layout:Graph.VMap.empty


let draw_graph_action graph_name () =
  let graph_widget =
    EditableGraph.create ~graph:(EditableGraph.get_graph init_graph) ~layout
  in
  let title = Dom_html.(createSpan document) in
  title##innerHTML <- Js.string "Draw a graph";
  Console.ask
    begin
      Question.question_of_form
        ~title
        ~form:(EditableGraph.edit_graph_form graph_widget)
    end
    (fun edgraph ->
       let graph = EditableGraph.get_graph edgraph in
       let layout = EditableGraph.get_layout edgraph in
       Console.add
         graph_name
         (graph,
          fun vertex ->
            Graph.VMap.unsafe_find vertex layout
         )
    )



let rename_action new_name old_name () =
  let open State.Infix in
  let action =
    if_defined old_name
    @@ fun graph ->
    Console.add new_name graph >>
    if old_name <> "last" then Console.remove old_name
    else State.return ()
  in
  action


let copy_action new_name old_name () =
  let open State.Infix in
  let action =
    if_defined old_name @@ fun old_graph ->
    Console.add new_name old_graph
  in
  action


let remove_action graph_name () =
  let open State.Infix in
  let action =
    if_defined graph_name @@ fun _ ->
    Console.remove graph_name
  in
  action

let run_action graph_name () =
  let open State.Infix in
  Console.find graph_name >>= function
  | None ->
    Console.send_messages
      Tree.(Node ("Undefined graph.", []))
  | Some value ->
    register_blossom graph_name value


let draw_command =
  let open Control.Line in
  Control.Argument.ident "name" "name of graph to be created"
  @* draw_graph_action


let rename_command =
  let open Control.Line in
  Control.Argument.ident "old_name" "name of graph to be renamed"
  @+ Control.Argument.ident "new_name" "new name for that heap"
  @* rename_action


let copy_command =
  let open Control.Line in
  Control.Argument.ident "original_name" "name of graph to be copied"
  @+ Control.Argument.ident "new_name" "new name for the copy"
  @* copy_action


let remove_command =
  let open Control.Line in
  Control.Argument.ident "name" "name of graph to be removed"
  @* remove_action

let run_command =
  let open Control.Line in
  Control.Argument.ident
    "name"
    "name of graph on which algorithm should run"
  @* run_action


let command =
  let open Control.Command in
  empty
  ++ register ~doc:"rename a heap" "rename" rename_command
  ++ register ~doc:"copy a heap" "copy" copy_command
  ++ register ~doc:"remove a heap" "remove" remove_command
  ++ register ~doc:"draw a graph" "draw" draw_command
  ++ register ~doc:"run algorithm" "run" run_command


let () =
  Console.on_submit_click
    begin
      let open State.Infix in
      let open Control.Command in
      Console.get_input >>= fun input_text ->
      let input_stream = MoreStream.from_string input_text in
      let comp = Control.Command.execute command input_stream in
      Console.send_messages comp.printed >>
      match comp.computed with
      | Some action -> action ()
      | None -> State.return ()
    end

let () =
  Console.start_event_loop
    (JsContext.init_context ())
