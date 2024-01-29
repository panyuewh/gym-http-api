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
    let string_of_body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    if Http.Status.compare resp.status `OK = 0 then
      string_of_body
    else
      raise (Error ("Rest", Format.sprintf "[POST %s] %d: %s" method_ (Http.Status.to_int resp.status) string_of_body))

  let get ~sw ~client base_url method_ params =
    let uri = Uri.of_string (base_url ^ method_ ^ params) in
    let headers =
      let h = Cohttp.Header.init () in
      (* let h = Cohttp.Header.add_authorization h (`Basic (wcs_cred.cred_username, wcs_cred.cred_password)) in *)
      let h = Cohttp.Header.add h "Content-Type" "application/json" in
      h
    in
    let resp, body = Cohttp_eio.Client.get ~sw client ~headers uri in
    let string_of_body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    if Http.Status.compare resp.status `OK = 0 then
      string_of_body
    else
      raise (Error ("Rest", Format.sprintf "[GET %s] %d: %s" method_ (Http.Status.to_int resp.status) string_of_body))

  let delete ~sw ~client base_url method_ =
    let uri = Uri.of_string (base_url ^ method_) in
    let headers =
      let h = Cohttp.Header.init () in
      (* let h = Cohttp.Header.add_authorization h (`Basic (wcs_cred.cred_username, wcs_cred.cred_password)) in *)
      let h = Cohttp.Header.add h "Content-Type" "application/json" in
      h
    in
    let resp, body = Cohttp_eio.Client.get ~sw client ~headers uri in
    let string_of_body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    if Http.Status.compare resp.status `OK = 0 then
      string_of_body
    else
      raise (Error ("Rest", Format.sprintf "[DELETE %s] %d: %s" method_ (Http.Status.to_int resp.status) string_of_body))
end
