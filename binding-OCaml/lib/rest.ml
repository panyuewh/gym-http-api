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

exception Error of string * string

module RestClient = struct

  (** {3 Utility functions} *)

  (* let parameters_of_json (o: json) : string =
     begin match o with
     | `Assoc [] -> ""
     | `Assoc ((x, v) :: l) ->
         let params = "?"^x^"="^(Yojson.Basic.to_string v) in
         List.fold_left
           (fun params (x, v) ->
              params^"&"^x^"="^(Yojson.Basic.to_string v))
           params l
     | _ -> raise (Error ("Rest", (Format.sprintf "parameters_of_json %s : json object expected" (Yojson.Basic.pretty_to_string o))))
     end *)
  
  let body_or_error (resp: Http.Response.t) body =
    let string_of_body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    match resp.status with
    | `OK | `No_content -> string_of_body
    | _ ->
      raise (Error ("Rest", Format.sprintf "%d: %s" (Http.Status.to_int resp.status) string_of_body))


  (** {3 Generic functions} *)


  let post ~sw ~client base_url method_ req =
    let uri = Uri.of_string (base_url ^ method_) in
    let headers =
      let h = Cohttp.Header.init () in
      (* let h = Cohttp.Header.add_authorization h (`Basic (wcs_cred.cred_username, wcs_cred.cred_password)) in *)
      let h = Cohttp.Header.add h "Content-Type" "application/json" in
      h
    in
    let data = Cohttp_eio.Body.of_string req  in
    let resp, body = Cohttp_eio.Client.post ~sw client ~body:data ~headers uri in
    Logs.info (fun m -> m "POST %s" method_);
    try body_or_error resp body with 
    | Error (sub, msg) ->
        Logs.err (fun m -> m "[POST %s] %s" method_ msg); 
        raise (Error (sub, Format.sprintf "[POST %s] %s" method_ msg))

  let get ~sw ~client base_url method_ params =
    let uri = Uri.of_string (base_url ^ method_ ^ params) in
    let headers =
      let h = Cohttp.Header.init () in
      (* let h = Cohttp.Header.add_authorization h (`Basic (wcs_cred.cred_username, wcs_cred.cred_password)) in *)
      let h = Cohttp.Header.add h "Content-Type" "application/json" in
      h
    in
    let resp, body = Cohttp_eio.Client.get ~sw client ~headers uri in
    Logs.info (fun m -> m "GET %s" method_);
    try body_or_error resp body with 
    | Error (sub, msg) -> 
        Logs.err (fun m -> m "[GET %s] %s" method_ msg); 
        raise (Error (sub, Format.sprintf "[GET %s] %s" method_ msg))

  let delete ~sw ~client base_url method_ =
    let uri = Uri.of_string (base_url ^ method_) in
    let headers =
      let h = Cohttp.Header.init () in
      (* let h = Cohttp.Header.add_authorization h (`Basic (wcs_cred.cred_username, wcs_cred.cred_password)) in *)
      let h = Cohttp.Header.add h "Content-Type" "application/json" in
      h
    in
    let resp, body = Cohttp_eio.Client.get ~sw client ~headers uri in
    Logs.info (fun m -> m "DELETE %s" method_);
    try body_or_error resp body with 
    | Error (sub, msg) -> 
        Logs.err (fun m -> m "[DELETE %s] %s" method_ msg); 
        raise (Error (sub, Format.sprintf "[DELETE %s] %s" method_ msg))
end
