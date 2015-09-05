

let tile_size = 32
let number_of_tiles = 20
let width = number_of_tiles * tile_size
let height = number_of_tiles * tile_size
let number_of_players = 11
let number_of_criminals = 22

type gameobject = {
  mutable x: int;
  mutable y: int;
  width: int;
  height: int;
  deadly: bool;
  blocking: bool;
  is_portal: bool;
  mutable image: Dom_html.imageElement Js.t;
}


type player = {
  name: string;
  moves: (int * int) list;
  image_name: string;
}

let available_players = [
  {
    name = "fred";
    moves = [(1,-1); (-1,-1); (2,-2); (-2,-2); (0,1); (0,-1)];
    image_name = "fred.png";
  };
  {
    name = "rapptz";
    moves = [(1,-1); (-1,-1); (1,1); (-1,1); (0,-1); (0, 1)];
    image_name = "rapptz.png";
  };
  {
    name = "abhishek";
    moves = [(0,1); (0,-1); (1,0); (-1,0)];
    image_name = "abhishek.jpg";
  };
  {
    name = "irxmaxell";
    moves = [(0,1); (0,-1); (1,0); (-1,0); (0,2); (0,-2); (2,0); (-2,0)];
    image_name = "irxmaxell.jpeg";
  };
  {
    name = "rlemon";
    moves = [(0,1); (0,-1); (1,0); (-1,0); (0,3); (0,-3); (3,0); (-3,0); (0,5); (0,-5); (5,0); (-5,0)];
    image_name = "rlemon.jpg";
  };
  {
    name = "jandvorak";
    moves = [(1, -2); (-1, -2); (1,2); (-1,2)];
    image_name = "jandvorak.png";
  };
  {
    name = "robot";
    moves = [(0,3); (0,-3); (3,0); (-3,0); (0,5); (0,-5); (5,0); (-5,0); (0,7); (0,-7); (7,0); (-7,0)];
    image_name = "robot.jpeg";
  };
  {
    name = "badger";
    moves = [(0,1); (0,-1); (1,0); (-1,0); (1, -2); (-1, -2)];
    image_name = "badger.png";
  };
]


type piece =
  | Player of player
  | Empty
  | Criminal
  | Meal


let selected_tile = ref (0, 0)
let board =
  let mkrow _ = Array.init number_of_tiles (fun _ -> Empty) in
  Array.init number_of_tiles mkrow

let round_number = ref 1

let initial_fall_speed = 4.0
let fall_accel = 0.02
let move_speed = 4
let fall_speed = ref initial_fall_speed

let key_left = 65
let key_right = 68


let make_image src =
  let img = Dom_html.createImg Dom_html.document in
  img##src <- Js.string src;
  img

let objects = []
(*
  List.flatten (List.map generate_objects object_types)
*)

module ImageMap = Map.Make(String)
let loaded_images = Hashtbl.create 100


let background_image = make_image "img/grass.jpg"

let test_image = make_image "img/bartek.jpeg"
let criminal_image = make_image "img/criminal.jpg"
let meal_image = make_image "img/cake.gif"
let gray_rectangle = make_image "img/gray_rectangle.png"

let debug_error str = Firebug.console##error (str);;
let debug_print str = Firebug.console##log (str);;
let print_exn exn = debug_error (Js.string (Printexc.to_string exn))

let catching_bind t next handle_exn =
  Lwt.bind t (fun () -> Lwt.catch next (fun exn -> handle_exn exn; Lwt.return_unit))

let get_canvas () =
  let c = Dom_html.getElementById "c" in
  let c = Js.Opt.get (Dom_html.CoerceTo.canvas c) (fun () -> failwith "no canvas") in
  let ctx = c##getContext (Dom_html._2d_) in
  c##width <- width;
  c##height <- height;
  ctx

let update_round_number s =
  let c = Dom_html.getElementById "round" in
  let c = Js.Opt.get (Dom_html.CoerceTo.element c) (fun () -> failwith "update_round_number") in
  c##textContent <- Js.Opt.return (Js.string s)

let draw_object obj ctx x y =
  let tx = x * tile_size in
  let ty = y * tile_size in
  begin match obj with
  | Empty -> ()
  | Player p ->
      let image = if Hashtbl.mem loaded_images p.name then
        Hashtbl.find loaded_images p.name
      else begin
        let i = make_image ("img/" ^ p.image_name) in
        Hashtbl.add loaded_images p.name i;
        i
      end in
      ctx##drawImage (image, float_of_int tx, float_of_int ty);
  | Criminal ->
      ctx##drawImage (criminal_image, float_of_int tx, float_of_int ty);
  | Meal ->
      ctx##drawImage (meal_image, float_of_int tx, float_of_int ty);
  end;
  if !selected_tile = (x, y) then begin
    ctx##drawImage (gray_rectangle, float_of_int tx, float_of_int ty)
  end

let draw_moves ctx x y p =
  let draw_move (mx, my) =
    ctx##drawImage (gray_rectangle, float_of_int ((x + mx) * tile_size), float_of_int ((y + my) * tile_size))
  in
  List.iter draw_move p.moves

let redraw ctx =
  ctx##drawImage (background_image, 0.0, 0.0);
  let draw_cell y x obj = draw_object obj ctx x y in
  let draw_row y row = CCArray.iteri (draw_cell y) row in
  CCArray.iteri draw_row board;
  let tx, ty = !selected_tile in
  match board.(ty).(tx) with
  | Player p -> draw_moves ctx tx ty p;
  | _ -> ()

let step ctx =
  redraw ctx;
  true

let rec loop ctx =
  catching_bind
    (Lwt_js.sleep 0.016)
    (fun () -> if step ctx then loop ctx else Lwt.return_unit)
    print_exn

let is_empty_tile (x, y) =
  match board.(y).(x) with
  | Empty -> true
  | _ -> false

let is_criminal_tile (x, y) =
  match board.(y).(x) with
  | Criminal -> true
  | _ -> false

let is_player_tile (x, y) =
  match board.(y).(x) with
  | Player _ -> true
  | _ -> false

let do_move from to_ =
  ()

let is_valid_move (from_x, from_y) (to_x, to_y) =
  let dx = to_x - from_x in
  let dy = to_y - from_y in
  dx >= 0 && dy >= 0 && dx < number_of_tiles && dy < number_of_tiles &&
  match board.(from_y).(from_x) with
  | Player p -> List.mem (dx, dy) p.moves
  | _ -> false

let player_move x y =
  let new_selected_tile = (x, y) in
  let cur_selected_tile = !selected_tile in
  let (cx, cy) = cur_selected_tile in
  let did_move = ref false in
  if is_valid_move cur_selected_tile new_selected_tile &&
     (is_empty_tile new_selected_tile || is_criminal_tile new_selected_tile) then
  begin
    board.(y).(x) <- board.(cy).(cx);
    board.(cy).(cx) <- Empty;
    did_move := true
  end;
  selected_tile := new_selected_tile;
  !did_move

let do_ai_move criminals =
  let move_criminal c =
    ()
  in
  List.iter move_criminal criminals

let unsafe_get_property obj str = Js.Unsafe.get obj (Js.string str)

let handle_click ctx players criminals ev _thread =
  debug_print ev;
  let x = unsafe_get_property ev "offsetX" / tile_size in
  let y = unsafe_get_property ev "offsetY" / tile_size in
  let did_move = player_move x y in
  if did_move then begin
    do_ai_move criminals;
    round_number := !round_number + 1;
    update_round_number (string_of_int !round_number)
  end;
  redraw ctx;
  Lwt.return_unit

let init_board team criminals b =
  b.(0).(number_of_tiles / 2) <- Meal;
  let put_player row i player =
    b.(row).(i * 2 + 1) <- Player player in
  let put_criminal (x, y) =
    b.(y).(x) <- Criminal in
  List.iteri (put_player 2) team;
  List.iter put_criminal criminals

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let rand_list count low high =
  CCRandom.run (CCRandom.replicate count (CCRandom.int_range low high))

let make_teams () =
  let players = CCList.take number_of_players (shuffle available_players) in
  let xs = rand_list number_of_criminals 0 (number_of_tiles - 1) in
  let ys = rand_list number_of_criminals (number_of_tiles - 4) (number_of_tiles - 1) in
  let criminals = List.combine xs ys in
(*
  let (players, criminals) = CCList.split 6 available_players in
*)
  (players, criminals)

let start _ =
  let ctx = get_canvas () in
  let player_team, criminal_team = make_teams () in
  init_board player_team criminal_team board;
  redraw ctx;
  Lwt.async (fun () ->
    Lwt_js_events.clicks Dom_html.window (handle_click ctx player_team criminal_team)
  );
  Js._false

let main () =
  Random.init (int_of_float (Js.math##random () *. 1000.0));
  let add el ev handler =
    ignore (Dom_html.addEventListener el ev (Dom_html.handler handler) Js._false)
  in
  add Dom_html.window Dom_html.Event.load start


let () = main ()

