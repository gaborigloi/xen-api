(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* Utilities for performance monitor *)

open Client
open Stdext.Xstringext

let rpc xml =
  let open Xmlrpc_client in
  XMLRPC_protocol.rpc ~srcstr:"perftest" ~dststr:"xapi" ~transport:(Unix (Filename.concat "/var/lib/xcp" "xapi")) ~http:(xmlrpc ~version:"1.0" "/") xml

let remoterpc host xml =
  let open Xmlrpc_client in
  XMLRPC_protocol.rpc ~srcstr:"perftest" ~dststr:"remotexapi" ~transport:(SSL(SSL.make (), host, 443)) ~http:(xmlrpc ~version:"1.1" "/") xml

let parse_sr_probe_for_iqn (xml: string) : string list =
  match Xml.parse_string xml with
  | Xml.Element("iscsi-target-iqns", _, children) ->
    let parse_tgts = function
      | Xml.Element("TGT", _, children) ->
        let parse_kv = function
          | Xml.Element(key, _, [ Xml.PCData v ]) ->
            key, String.strip String.isspace v (* remove whitespace at both ends *)
          | _ -> failwith "Malformed key/value pair" in
        let all = List.map parse_kv children in
        List.assoc "TargetIQN" all
      | _ -> failwith "Malformed or missing <TGT>" in
    List.map parse_tgts children
  | _ -> failwith "Missing <iscsi-target-iqns> element"

let parse_sr_probe_for_scsiids (xml : string) : string list =
  match Xml.parse_string xml with
  | Xml.Element("iscsi-target", _, children) ->
    let parse_luns = function
      | Xml.Element("LUN", _, children) ->
        let parse_kv = function
          | Xml.Element(key, _, [ Xml.PCData v ]) ->
            key, String.strip String.isspace v (* remove whitespace at both ends *)
          | _ -> failwith "Malformed key/value pair" in
        let all = List.map parse_kv children in
        List.assoc "SCSIid" all
      | _ -> failwith "Malformed or missing <LUN>" in
    List.map parse_luns children
  | _ -> failwith "Missing <iscsi-target> element"
