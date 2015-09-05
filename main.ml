

let tile_size = 32
let number_of_tiles = 20
let width = number_of_tiles * tile_size
let height = number_of_tiles * tile_size
let number_of_players = 14
let number_of_criminals = 40


type player = {
  name: string;
  moves: (int * int) list;
  image_name: string;
}

let available_players = [
{
  name = "FredOverflow";
  moves = [(1,1); (-1,1); (2,2); (-2,2); (0,1); (0,-1); (3,0); (-3,0)];
  image_name = "fred.png";
};
{
  name = "Rapptz";
  moves = [(1,-1); (-1,-1); (1,1); (-1,1); (0,-1); (0, 1); (5,0); (-5,0)];
  image_name = "rapptz.png";
};
{
  name = "Abhishek";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (5,0); (-5,0)];
  image_name = "abhishek.jpg";
};
{
  name = "ircmaxell";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (0,2); (0,-2); (2,0); (-2,0); (6,0); (-6,0)];
  image_name = "irxmaxell.jpeg";
};
{
  name = "rlemon";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (0,3); (0,-3); (3,0); (-3,0); (0,5); (0,-5); (5,0); (-5,0)];
  image_name = "rlemon.jpg";
};
{
  name = "Jan Dvorak";
  moves = [(1, -2); (-1, -2); (1,2); (-1,2); (7,0); (-7,0)];
  image_name = "jandvorak.png";
};
{
  name = "R. Martinho";
  moves = [(0,3); (0,-3); (3,0); (-3,0); (0,5); (0,-5); (5,0); (-5,0); (0,7); (0,-7); (7,0); (-7,0)];
  image_name = "robot.jpeg";
};
{
  name = "Badgercat";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (1, 2); (-1, 2); (3,0); (-3,0)];
  image_name = "badger.png";
};
{
  name = "Jerry Coffin";
  moves = [(1,1); (-1, -1); (2, 2); (-2, 2); (1,-1); (-1,1); (4,0); (-4,0)];
  image_name = "jerry.jpeg";
};
{
  name = "Madara Uchiha";
  moves = [(1,1); (-1,1); (2,2); (-2,2); (0,1); (0,-1); (3,0); (-3,0)];
  image_name = "secondrikudo.png";
};
{
  name = "Bartek";
  moves = [(1,1); (-1,1); (2,2); (-2,2); (0,1); (0,-1); (5,0); (-5,0)];
  image_name = "bartek.jpeg";
};
{
  name = "Jon Clements";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (1,1); (-1,1); (2,2); (-2,2); (5,0); (-5,0); (2,0); (-2,0)];
  image_name = "jonclements.jpg";
};
{
  name = "sehe";
  moves = [(0,1); (0,-1); (2,0); (-2,0); (0,4); (0,-4); (3,0); (-3,0); (8,0); (-8,0); (5,0); (-5,0)];
  image_name = "sehe.png";
};
{
  name = "Benjamin";
  moves = [(0,1); (0,2); (0,3); (0,4); (0,5); (0,6); (1,1); (-1,1); (0,-1); (3,0); (-3,0); (5,0); (-5,0)];
  image_name = "benjamin.jpeg";
};
{
  name = "Kendall Frey";
  moves = [(1,1); (-1,1); (2,5); (-2,1); (0,1);];
  image_name = "kendall.jpeg";
};
{
  name = "Some guy";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (0,2); (0,3); (0,4)];
  image_name = "someguy.png";
};
{
  name = "Caprica Six";
  moves = [(1, -2); (-1, -2); (1,2); (-1,2)];
  image_name = "caprica.jpg";
};
{
  name = "Tony the Lion";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (1,1); (-1,1); (2,2); (-2,2)];
  image_name = "lion.jpeg";
};
{
  name = "Somekittens";
  moves = [(0,2); (0,-2); (1,0); (-1,0); (0,1); (0,-1); (3,0); (-3,0)];
  image_name = "somekittens.png";
};
{
  name = "Cat Plus Plus";
  moves = [(1,1); (-1,1); (2,2); (-2,2); (0,1); (0,-1)];
  image_name = "cat.jpeg";
};
{
  name = "Loktar";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (0,2); (0,-2); (2,0); (-2,0)];
  image_name = "loktar.jpg";
};
{
  name = "StackedCrooked";
  moves = [(0,3); (0,-3); (3,0); (-3,0); (0,2); (0,-2); (2,0); (-2,0)];
  image_name = "stackedcrooked.png";
};
{
  name = "copy";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (3,-3); (-3,-3); (3,3); (-3,3); (5,-5); (-5,-5); (2,2); (-2,2)];
  image_name = "copy.png";
};
{
  name = "Lightness";
  moves = [(0,4); (0,-4); (4,0); (-4,0); (0,2); (0,-2); (2,0); (-2,0)];
  image_name = "lrio.jpeg";
};
{
  name = "Sterling Archer";
  moves = [(0,1); (0,-1); (1,0); (-1,0); (3,-3); (-3,-3); (3,3); (-3,3)];
  image_name = "sterling.jpg";
};
{
  name = "elyse";
  moves = [(1,1); (-1,1); (2,2); (-2,2); (0,1); (0,-1)];
  image_name = "elyse.png";
};
{
  name = "Mystical";
  moves = [(0,3); (0,-3); (3,0); (-3,0)];
  image_name = "mystical.jpg";
};
{
  name = "teresko";
  moves = [(1,0); (-0,1); (6,6); (-3,3); (0,1); (0,-1); (5,0); (-5,0)];
  image_name = "teresko.jpeg";
};
{
  name = "Etienne";
  moves = [(2, 1); (2,-1); (2,2); (2, -3); (2, 4); (2,-5); (2,6); (2,-7)];
  image_name = "etienne.jpeg";
};
{
  name = "Nooble";
  moves = [(0,1); (0,-1); (1,0); (-1,0)];
  image_name = "nooble.jpg";
};
{
  name = "Xeo";
  moves = [(1,1); (-1,1); (2,2); (-2,2); (3,1); (5,5); (0,1); (0,2); (-2,2); (3,1); (0,-1)];
  image_name = "xeo.png";
};
{
  name = "Florian";
  moves = [(2, 1); (2,-1); (2,2); (2, -3); (2, 4); (2,-5); (2,6); (2,-7)];
  image_name = "florian.jpeg";
};
{
  name = "Puppy";
  moves = [(0,1); (0,-1); (1,0); (-1,0)];
  image_name = "puppy.jpeg";
};
{
  name = "Zirak";
  moves = [(1,1); (-1,1); (2,2); (-2,2); (0,1); (0,-1)];
  image_name = "zirak.png";
};
]


type piece =
  | Player of player
  | Empty
  | Criminal
  | Cake


let selected_tile = ref (0, 0)
let board =
  let mkrow _ = Array.init number_of_tiles (fun _ -> Empty) in
  Array.init number_of_tiles mkrow

let round_number = ref 1
let game_over = ref false

let make_image src =
  let img = Dom_html.createImg Dom_html.document in
  img##src <- Js.string src;
  img

module ImageMap = Map.Make(String)
let loaded_images = Hashtbl.create 100


let background_image = make_image "img/grass.jpg"

let test_image = make_image "img/bartek.jpeg"
let criminal_image = make_image "img/criminal.jpg"
let cake_image = make_image "img/cake.gif"
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

let set_text id text =
  let c = Dom_html.getElementById id in
  let c = Js.Opt.get (Dom_html.CoerceTo.element c) (fun () -> failwith "set_text") in
  c##textContent <- Js.Opt.return (Js.string text)

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
  | Cake ->
      ctx##drawImage (cake_image, float_of_int tx, float_of_int ty);
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

let do_move (from_x, from_y) (to_x, to_y) =
  board.(to_y).(to_x) <- board.(from_y).(from_x);
  board.(from_y).(from_x) <- Empty

let is_valid_move (from_x, from_y) (to_x, to_y) =
  let dx = to_x - from_x in
  let dy = to_y - from_y in
  match board.(from_y).(from_x) with
  | Player p -> List.mem (dx, dy) p.moves
  | _ -> false

let player_move x y =
  let new_selected_tile = (x, y) in
  let cur_selected_tile = !selected_tile in
  let did_move = ref false in
  if is_valid_move cur_selected_tile new_selected_tile &&
     (is_empty_tile new_selected_tile || is_criminal_tile new_selected_tile) then
  begin
    do_move cur_selected_tile new_selected_tile;
    did_move := true;
    selected_tile := (0, 0)
  end else
    selected_tile := new_selected_tile;
  !did_move

let is_cake p = match p with
  | Cake -> true
  | _ -> false

let is_criminal = function
  | Criminal -> true
  | _ -> false

let do_ai_move () =
  let move_criminal y x p =
    match p with
    | Criminal ->
      if CCRandom.run (CCRandom.int_range 0 100) > 33 then
      begin
        let towards_mid = if x > number_of_tiles / 2 then -1 else 1 in
        if y = 0 then
          do_move (x, y) (x + towards_mid, y)
        else
          do_move (x, y) (x + towards_mid, y - 1)
      end
    | _ -> ()
  in
  let move_row y row =
    Array.iteri (move_criminal y) row
  in
  Array.iteri move_row board

let unsafe_get_property obj str = Js.Unsafe.get obj (Js.string str)

let board_has pred =
  CCArray.exists (fun row -> CCArray.exists pred row) board

let check_game_over () =
  not (board_has is_cake)

let check_is_win () =
  not (board_has is_criminal)

let handle_click ctx players criminals ev _thread =
  if not !game_over then begin
    let x = unsafe_get_property ev "offsetX" / tile_size in
    let y = unsafe_get_property ev "offsetY" / tile_size in
    let did_move = player_move x y in
    if did_move then begin
      do_ai_move ();
      round_number := !round_number + 1;
      set_text "round" (string_of_int !round_number)
    end;
    redraw ctx;
    if check_game_over () then begin
      game_over := true;
      Dom_html.window##alert (Js.string (Printf.sprintf "The cake was stolen! You lasted %d rounds." !round_number));
    end;
    if check_is_win () then begin
      game_over := true;
      Dom_html.window##alert (Js.string (Printf.sprintf "The bandits were defeated! It took you %d rounds." !round_number));
    end
  end;
  Lwt.return_unit

let init_board team criminals b =
  b.(0).(number_of_tiles / 2) <- Cake;
  let put_player row i player =
    b.(row).(i * 2 + row mod 2 + 1) <- Player player in
  let put_criminal (x, y) =
    b.(y).(x) <- Criminal in
  let (t0, t1) = CCList.split (number_of_players / 2) team in
  List.iteri (put_player 2) t0;
  List.iteri (put_player 3) t1;
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
  let ys = rand_list number_of_criminals (number_of_tiles - 7) (number_of_tiles - 1) in
  let criminals = List.combine xs ys in
  (players, criminals)

let start _ =
  let ctx = get_canvas () in
  let player_team, criminal_team = make_teams () in
  init_board player_team criminal_team board;
  set_text "players" (String.concat ", " (List.map (fun p -> p.name) player_team));
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

