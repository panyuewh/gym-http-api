open OUnit2
(* open Gym_client *)

let tests = "test suite" >::: [
  "empty" >:: (fun _ -> assert_equal 0 0);
  "singleton" >:: (fun _ -> assert_equal 1 1);
]

let _ = run_test_tt_main tests
(*
Eio_mock.Backend.run @@ fun () ->
  let net = Eio_mock.Net.make "mocknet" in
  let flow = Eio_mock.Flow.make "flow" in
  Eio_mock.Net.on_connect net [`Return flow];
  Eio_mock.Flow.on_read flow [
    `Return "(packet 1)";
    `Yield_then (`Return "(packet 2)");
    `Raise End_of_file;
  ];
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  run_client ~net ~addr;;
  
let instance_id =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_create@.";
  let instance_id = Gym_client.env_create "CartPole-v1" in
  Format.printf "%s@." instance_id;
  instance_id

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_list_all@.";
  let envs = Gym_client.env_list_all () in
  List.iter
    (fun (instance_id, env_id) -> Format.printf "  %s: %s@." instance_id env_id)
    envs

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_reset@.";
  let obs = Gym_client.env_reset instance_id in
  Format.print_string "observation = ";
  List.iter Format.print_float obs

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_step@.";
  let obs, rew, is_done, _ = Gym_client.env_step instance_id (Int 0) false in
  List.iter Format.print_float obs;
  Format.printf "%f %b." rew is_done

let print_space_info_ (x : space_info_typ) =
  match x with
  | Disc n -> Format.printf "Discrete: n=%d" n
  | Box (shape, lows, highs) ->
      Format.print_string "Box: ";
      List.iter Format.print_int shape;
      List.iter Format.print_float lows;
      List.iter Format.print_float highs

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_action_space_info@.";
  let resp = Gym_client.env_action_space_info instance_id in
  match resp with
  | Disc n -> Format.printf "Discrete, n=%d" n
  | Box (shape, lows, highs) ->
      List.iter Format.print_int shape;
      List.iter Format.print_float lows;
      List.iter Format.print_float highs

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_action_space_sample@.";
  let resp = Gym_client.env_action_space_sample instance_id in
  Format.print_string "action = .";
  match resp with
  | Int x -> Format.print_int x
  | Floats y -> List.iter Format.print_float y

(* let () =
   Format.printf "-------------------------------@.";
   Format.printf "Test env_action_space_contains@.";
   let resp = Gym_client.env_action_space_contains instance_id 0 in
   Format.printf "member = %s@." (string_of_bool resp) *)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_observation_space_info@.";
  let resp = Gym_client.env_observation_space_info instance_id in
  print_space_info_ resp

(* let () =
   Format.printf "-------------------------------@.";
   Format.printf "Test env_observation_space_contains@.";
   let resp =
     Gym_client.env_observation_space_contains instance_id (`Assoc[])
   in
   Format.printf "member = %s@." (string_of_bool resp) *)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_monitor_start@.";
  let () =
    Gym_client.env_monitor_start instance_id "/tmp/gym-results" true false
  in
  Format.printf "monitor started@."

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_monitor_close@.";
  let () = Gym_client.env_monitor_close instance_id in
  Format.printf "monitor closed@."

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_close@.";
  let () = Gym_client.env_close instance_id in
  Format.printf "closed %s@." instance_id

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_list_all@.";
  let envs = Gym_client.env_list_all () in
  List.iter
    (fun (instance_id, env_id) -> Format.printf "  %s: %s@." instance_id env_id)
    envs
 *)
(* let () = *)
(*   Format.printf "-------------------------------@."; *)
(*   Format.printf "Test shutdown@."; *)
(*   let resp = Gym_client.shutdown_server () in *)
(*   Format.printf "server shutdowned: %s@." resp *)
