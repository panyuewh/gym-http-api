(*
 *  This file is part of the gym-http-api OCaml binding project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

(* open Yojson *)
(* open Rest *)


type safe = Yojson.Safe.t
type basic = Yojson.Basic.t
type json = basic
type lexer_state = Yojson.Basic.lexer_state

let write_json = Yojson.Basic.write_t
let read_json = Yojson.Basic.read_t
let from_string j = Yojson.Basic.from_string j
let to_string j = Yojson.Basic.to_string j
let string_of j = Yojson.Basic.Util.to_string j
let int_of j = Yojson.Basic.Util.to_int j
let float_of j = Yojson.Basic.Util.to_float j
let bool_of j = Yojson.Basic.Util.to_bool j
let list_of j = Yojson.Basic.Util.to_list j
let assoc_of j = Yojson.Basic.Util.to_assoc j
let member s j = Yojson.Basic.Util.member s j

type act_typ = Int of int | Floats of float list 
type space_info_typ = Disc of int | Box of int list * float list * float list

include Rest.Make(Cohttp_lwt_unix.Client)

(** Server url: default is [http://127.0.0.1:5000] *)
let base_url = ref "http://127.0.0.1:5000"

(** [env_create env_id] creates an instance of the specified
    environment (e.g., ["CartPole-v1"]). It returns the instance
    identifier.
*)
let env_create : ?render_mode:string -> string -> string = begin
  fun ?(render_mode="rgb_array") env_id ->
    let method_ = "/v1/envs/" in
    (* let req = string_of_env_id { env_id = env_id; mode = mode } in *)
    let req : json = `Assoc [("env_id", `String env_id); ("render_mode", `String render_mode)] in
    let rsp : json = from_string (post !base_url method_ (to_string req)) in
    rsp |> member "instance_id" |> string_of
end

(** [env_list_all ()] lists all the environments running on the server
    as a pair [(instance_id, env_id)] (e.g. [[("3c657dbc", "CartPole-v1")]]).
*)
let env_list_all : unit -> (string * string) list = begin
  fun () ->
    let method_ = "/v1/envs/" in
    let params = "" in
    let rsp : json = from_string (get !base_url method_ params) in
    List.map
      (fun (instance_id, env_id) -> (instance_id, (to_string env_id)))
      (member "all_envs" rsp |> assoc_of)
end

(** [env_reset instance_id] resets the state of the environment and
    return an initial observation.
*)
let env_reset : string -> float list = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id^"/reset/" in
    let req = "" in
    let rsp = from_string (post !base_url method_ req) in
    print_string (to_string rsp);
    rsp |> member "observation" |> list_of |> List.map float_of
    (* List.map float_of (list_of (member "observation" rsp)) *)
end

(** [env_step instance_id action render] steps though an environment
    using an action. If [render] is true, a graphical feedback if
    display by the server.
*)
let env_step : string -> act_typ -> bool -> float list * float * bool * json = begin
  fun instance_id action render ->
    let method_ = "/v1/envs/"^instance_id^"/step/" in
    let req = `Assoc [
      ("render", `Bool render);
      ("action", (match action with
          Int x -> `Int x
        | Floats y-> `List (List.map (fun x -> `Float x) y)))]
    in
    let rsp = from_string (post !base_url method_ (to_string req)) in
    ( rsp |> member "observation" |> list_of |> List.map float_of, 
      rsp |> member "reward" |> float_of, 
      rsp |> member "done" |> bool_of, 
      rsp |> member "info" )
end

let space_info_: json -> space_info_typ = fun info ->
begin 
    match string_of (member "name" info) with 
      "Discrete" -> Disc (int_of (member "n" info))
    | "Box" -> Box (
        (info |> member "shape" |> list_of |> List.map int_of),
        (info |> member "low" |> list_of |> List.map float_of),
        (info |> member "high" |> list_of |> List.map float_of)
      )
    | _ -> failwith "Unexpected value"
end


(** [env_action_space_info instance_id] gets information (name and
    dimensions/bounds) of the env's action_space.
*)
let env_action_space_info : string -> space_info_typ = begin
  fun instance_id  ->
    let method_ = "/v1/envs/"^instance_id^"/action_space/" in
    let params = "" in
    let rsp = from_string (get !base_url method_ params) in
    space_info_ (member "info" rsp)
end

(** [env_action_space_sample instance_id] samples randomly from the
    env's action_space.
*)
let env_action_space_sample : string -> act_typ = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id^"/action_space/sample" in
    let params = "" in
    let rsp = from_string (get !base_url method_ params) in
    (* rsp |> to_list |> List.map to_float  *)
    match rsp |> member "action" with
      `Int x -> Int x
    | `List y -> Floats (List.map float_of y)
    | _ -> print_endline (to_string rsp); failwith "Unexpected action type"
end

(** [env_action_space_contains instance_id x] checks to see if the
    value [x] is valid in the env's action_space.
*)
(* let env_action_space_contains : string -> int -> bool = begin
  fun instance_id x ->
    let method_ =
      "/v1/envs/"^instance_id^"/action_space/contains/"^(string_of_int x)
    in
    let params = "" in
    let rsp = from_string(get !base_url method_ params) in
      true (* TODO *)
end *)

(** [env_observation_space_info instance_id] gets information (name
    and dimensions/bounds) of the env's observation_space.
*)
let env_observation_space_info : string -> space_info_typ = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id^"/observation_space/" in
    let params = "" in
    let rsp = from_string (get !base_url method_ params) in
    print_endline (to_string rsp);
    space_info_ (member "info" rsp)
end

(** [env_observation_space_contains instance_id params] assesses that
    the parameters are members of the env's observation_space.
*)
(* let env_observation_space_contains : string -> json -> bool = begin
  fun instance_id req ->
    let method_ = "/v1/envs/"^instance_id^"/observation_space/contains" in
    let rsp = from_string (post !base_url method_ (to_string req)) in
    true (* TODO *)
end *)

(** [env_monitor_start instance_id directory force resume] starts
    monitoring. [force] clears out existing training data from this
    directory (by deleting every file prefixed with [openaigym.]).
    [resume] retains the training data already in this directory,
    which will be merged with our new data.
*)
let env_monitor_start : string -> string -> bool -> bool -> unit = begin
  fun instance_id directory force resume ->
    let method_ = "/v1/envs/"^instance_id^"/monitor/start/" in
    let req = `Assoc [
      ("directory", `String directory);
      ("monitor_force", `Bool force);
      ("monitor_resume", `Bool resume);
      ("monitor_video_callable", `Bool false)] 
    in
    let _rsp = post !base_url method_ (to_string req) in
    assert (_rsp = "");
    ()
end

(** [env_monitor_close instance_id] flushes all monitor data to disk.
*)
let env_monitor_close : string -> unit = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id^"/monitor/close/" in
    let req = "" in
    let _rsp = post !base_url method_ req in
    assert (_rsp = "");
    ()
end

(** [env_close instance_id] stops the environment. *)
let env_close : string -> unit = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id^"/close/" in
    let req = "" in
    let _rsp = post !base_url method_ req in
    assert (_rsp = "");
    ()
end

(** [shutdown_server ()] requests a server shutdown. *)
let shutdown_server : unit -> string = begin
  fun () ->
    let method_ = "/v1/shutdown/" in
    let req = "" in
    let rsp = post !base_url method_ req in
    rsp
end
