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

(** Generate OCaml code to access the backend database *)
open Stdext.Listext

module O = Ocaml_syntax
module OU = Ocaml_utils
module Client = Gen_client
module DT = Datamodel_types
module DU = Datamodel_utils
module DM = Datamodel
open DT

(* Names of the modules we're going to generate (use these to prevent typos) *)
let _dm_to_string = "DM_to_String"
let _string_to_dm = "String_to_DM"
let _db_action    = "DB_Action"
let _db_defaults  = "DB_DEFAULTS"

(** Filter out all the operations which don't make sense to the database *)
let make_db_api = Dm_api.filter (fun _ -> true) (fun _ -> true)
    (fun ({ msg_tag = tag }) -> match tag with
       | FromField(_, _) -> true
       | Custom -> false
       | FromObject(GetAll) -> false (* rely on the Private(GetDBAll) function for now *)
       | FromObject(_) -> true
    )

(* Only these types are actually marshalled into the database: *)
let type_marshalled_in_db = function
  | Record _ -> false
  | Map(_, Record _) -> false
  | Set(Record _) -> false
  | _ -> true

(** Generate a module of datamodel type to string marshalling functions. *)
let dm_to_string tys : O.Module.t =
  let tys = List.filter type_marshalled_in_db tys in
  (* For every type, we create a single function *)
  let ty_fun ty =
    let body = match ty with
      | DT.Bool -> "string_of_bool"
      | DT.DateTime -> "fun x -> (try Date.assert_utc x with Invalid_argument s -> raise (DateTimeError s)); Date.to_string x"
      | DT.Enum(name, cs) ->
        let aux (c, _) = (OU.constructor_of c)^" -> \""^c^"\"" in
        "\n    fun v -> match v with\n      "^
        String.concat "\n    | " (List.map aux cs)
      (* ^"\n    | _ -> raise (StringEnumTypeError \""^name^"\")" *)
      | DT.Float -> "Printf.sprintf \"%0.18g\""
      | DT.Int -> "Int64.to_string"
      | DT.Map(key, value) ->
        let kf = OU.alias_of_ty key and vf = OU.alias_of_ty value in
        "fun m -> map "^kf^" "^vf^" m"
      | DT.Ref s -> "(Ref.string_of : " ^ OU.ocaml_of_ty ty^" -> string)"
(*
      | DT.Ref "session" -> "(Uuid.string_of_cookie : "^OU.ocaml_of_ty ty^" -> string)"
      | DT.Ref s -> "(Uuid.string_of_uuid : "^OU.ocaml_of_ty ty^" -> string)"
*)
      | DT.Set ty -> "fun s -> set "^OU.alias_of_ty ty^" s"
      | DT.String -> "fun x -> x"
      | DT.Record _ -> "(* record types never stored in the database *)" in
    O.Let.make
      ~name:(OU.alias_of_ty ty)
      ~params:[]
      ~ty:"string"
      ~body:[ body ] () in

  O.Module.make
    ~name:_dm_to_string
    ~preamble: [ "exception StringEnumTypeError of string";
                 "exception DateTimeError of string";
                 "open String_marshall_helper" ]
    ~letrec:true
    ~elements:(List.map (fun ty -> O.Module.Let (ty_fun ty)) tys) ()


(** Generate a module of string to datamodel type unmarshalling functions *)
let string_to_dm tys : O.Module.t =
  let tys = List.filter type_marshalled_in_db tys in
  (* For every type, we create a single function *)
  let ty_fun ty =
    let body = match ty with
      | DT.Bool -> "bool_of_string"
      | DT.DateTime -> "fun x -> Date.of_string x"
      | DT.Enum(name, cs) ->
        let aux (c, _) = "\""^c^"\" -> "^(OU.constructor_of c) in
        "\n    fun v -> match v with\n      "^
        String.concat "\n    | " (List.map aux cs)^
        "\n    | _ -> raise (StringEnumTypeError \""^name^"\")"
      | DT.Float -> "float_of_string"
      | DT.Int -> "Int64.of_string"
      | DT.Map(key, value) ->
        let kf = OU.alias_of_ty key and vf = OU.alias_of_ty value in
        "fun m -> map "^kf^" "^vf^" m"
(*
      | DT.Ref "session" ->
          "fun x -> (Uuid.cookie_of_string x : "^OU.ocaml_of_ty ty^")"
      | DT.Ref s -> "fun x -> (Uuid.uuid_of_string x : "^OU.ocaml_of_ty ty^")"
*)
      | DT.Ref s -> "fun x -> (Ref.of_string x : "^OU.ocaml_of_ty ty^")"
      | DT.Set ty -> "fun s -> set "^OU.alias_of_ty ty^" s"
      | DT.String -> "fun x -> x"
      | DT.Record _ -> "(* record types never stored in the database *)" in
    O.Let.make
      ~name:(OU.alias_of_ty ty)
      ~params:[]
      ~ty:(OU.alias_of_ty ty)
      ~body:[ body ] () in

  O.Module.make
    ~name:_string_to_dm
    ~preamble: [ "exception StringEnumTypeError of string";
                 "open String_unmarshall_helper" ]
    ~letrec:true
    ~elements:(List.map (fun ty -> O.Module.Let (ty_fun ty)) tys) ()

(** True if a field is actually in this table, false if stored elsewhere
    (ie Set(Ref _) are stored in foreign tables *)
let field_in_this_table = function
  | { DT.ty = DT.Set(DT.Ref _); DT.field_ignore_foreign_key = false } -> false
  | _ -> true

(* the function arguments are similar to the client, except the Make *)
let args_of_message (obj: obj) ( { msg_tag = tag } as msg) =
  let arg_of_param = function
    | {param_type=DT.Record x; param_name=name; param_doc=doc} ->
      begin match tag with
        | FromObject(Make) ->
          if x <> obj.DT.name then failwith "args_of_message";
          (* Client constructor takes all object fields regardless of qualifier
             	       but excluding Set(Ref _) types *)
          let fields = DU.fields_of_obj obj in
          let fields = List.filter field_in_this_table fields in
          List.map Client.param_of_field fields
        | _ -> failwith "arg_of_param: encountered a Record in an unexpected place"
      end
    | p -> [ Client.of_param p ] in
  let ref = if tag = FromObject(Make) then [ O.Named("ref", OU.alias_of_ty (Ref obj.name)) ] else [ ] in
  let args = List.map arg_of_param msg.msg_params in

  List.concat (ref :: args)

(** True if a field is in the client side record (ie not an implementation field) *)
let client_side_field f = not (f.DT.internal_only)

let get_record (obj: obj) aux_fn_name =
  let body =
    [
      Printf.sprintf "let (__regular_fields, __set_refs) = DB.read_record __t \"%s\" %s in"
        (Escaping.escape_obj obj.DT.name) Client._self;
      aux_fn_name^" ~__regular_fields ~__set_refs";
    ] in
  String.concat "\n" body

(* This function is incomplete:
   let make_shallow_copy api (obj: obj) (src: string) (dst: string) (all_fields: field list) =
   (* NB this copy does not include Set(Ref _) fields, and nor does it call any
     custom actions of other (Ref _) fields! *)
   let fields = List.filter field_in_this_table all_fields in
   let fields = List.filter (fun x -> x.full_name <> [ "uuid" ]) fields in
   let sql_fields = List.map (fun f -> Escaping.escape_id f.full_name) fields in
   let to_notify = follow_references obj api in
   let to_notify' = List.map
    (fun (tbl, fld) ->
       tbl, "\"" ^ (Escaping.escape_id fld.full_name) ^ "\"", "(fun () -> failwith \"shallow copy\")") to_notify in
    Printf.sprintf "sql_copy %s ~new_objref:%s \"%s\" %s [%s]"
      (ocaml_of_tbl_fields to_notify')
      dst
      (Escaping.escape_obj obj.DT.name) src
      (String.concat "; " (List.map (fun f -> "\"" ^ f ^ "\"") sql_fields))
*)

let open_db_module =
  "let __t = Context.database_of __context in\n" ^
  "let module DB = (val (Db_cache.get __t) : Db_interface.DB_ACCESS) in\n"

let db_action api : O.Module.t =
  let api = make_db_api api in

  let expr = "expr" in
  let expr_arg = O.Named(expr, "Db_filter_types.expr") in

  let get_refs_where (obj: obj) =
    let tbl = Escaping.escape_obj obj.DT.name in
    O.Let.make
      ~name: "get_refs_where"
      ~params: [ Gen_common.context_arg; expr_arg ]
      ~ty: ( OU.alias_of_ty (Ref obj.DT.name) ^ " list")
      ~body: [ open_db_module; "let refs = (DB.find_refs_with_filter __t \"" ^ tbl ^ "\" " ^ expr ^ ") in ";
               "List.map Ref.of_string refs " ] () in

  let get_record_aux_fn_body ?(m="API.") (obj: obj) (all_fields: field list) =

    let of_field = function
      | { DT.ty = DT.Set(DT.Ref other); full_name = full_name; DT.field_ignore_foreign_key = false } ->
        Printf.sprintf "List.map %s.%s (List.assoc \"%s\" __set_refs)"
          _string_to_dm
          (OU.alias_of_ty (DT.Ref other))
          (Escaping.escape_id full_name)
      | f ->
        _string_to_dm ^ "." ^ (OU.alias_of_ty f.DT.ty) ^
        "(List.assoc \"" ^ (Escaping.escape_id f.full_name) ^ "\" __regular_fields)" in
    let make_field f = Printf.sprintf "        %s%s = %s;" m (OU.ocaml_of_record_field (obj.DT.name :: f.DT.full_name)) (of_field f) in
    let fields = List.map make_field all_fields in
    let mk_rec = [ "{" ] @ fields @ [ "    }"] in
    String.concat "\n" mk_rec in

  let get_record_aux_fn (obj : obj) =
    let record_fields = List.filter client_side_field (DU.fields_of_obj obj) in
    O.Let.make
      ~name: "get_record'"
      ~params: [ O.Named("__regular_fields", "(string * string) list");
                 O.Named("__set_refs", "(string * (string list)) list") ]
      ~ty:"'a"
      ~body: [ get_record_aux_fn_body obj record_fields ] () in

  let get_record_internal_aux_fn (obj : obj) =
    let record_fields = DU.fields_of_obj obj in
    O.Let.make
      ~name: "get_record_internal'"
      ~params: [ O.Named("__regular_fields", "(string * string) list");
                 O.Named("__set_refs", "(string * (string list)) list") ]
      ~ty: "'a"
      ~body: [ get_record_aux_fn_body ~m:"" obj record_fields ] () in

  let get_records_where (obj: obj) name conversion_fn =
    O.Let.make
      ~name: name
      ~params: [ Gen_common.context_arg; expr_arg ]
      ~ty: ("'a")
      ~body: [ open_db_module;
               Printf.sprintf "let records = DB.read_records_where __t \"%s\" %s in"
                 (Escaping.escape_obj obj.DT.name) expr;
               Printf.sprintf "List.map (fun (ref,(__regular_fields,__set_refs)) -> Ref.of_string ref, %s __regular_fields __set_refs) records" conversion_fn] () in

  let register_get_record obj = O.Let.make
      ~name:"_"
      ~params:[]
      ~ty:"unit"
      ~body:[
        Printf.sprintf "Hashtbl.add Eventgen.get_record_table \"%s\"" obj.DT.name;
        Printf.sprintf "(fun ~__context ~self -> (fun () -> API.rpc_of_%s_t (%s.get_record ~__context ~self:(Ref.of_string self))))"
          (OU.ocaml_of_record_name obj.DT.name)
          (OU.ocaml_of_obj_name obj.DT.name)
      ]
      () in

  let operation (obj: obj) ( { msg_tag = tag } as x ) =
    let args = args_of_message obj x in

    let to_string arg =
      let binding = O.string_of_param arg in
      let converter = O.type_of_param arg in
      Printf.sprintf "let %s = %s.%s %s in" binding _dm_to_string converter binding in


    let body = match tag with
      | FromField(Setter, fld) ->
        Printf.sprintf "DB.write_field __t \"%s\" %s \"%s\" value"
          (Escaping.escape_obj obj.DT.name)
          Client._self
          (Escaping.escape_id fld.DT.full_name)
      | FromField(Getter, { DT.ty = ty; full_name = full_name }) ->
        Printf.sprintf "%s.%s (DB.read_field __t \"%s\" \"%s\" %s)"
          _string_to_dm (OU.alias_of_ty ty)
          (Escaping.escape_obj obj.DT.name)
          (Escaping.escape_id full_name)
          Client._self
      | FromField(Add, { DT.ty = DT.Map(_, _); full_name = full_name }) ->
        Printf.sprintf "DB.process_structured_field __t (%s,%s) \"%s\" \"%s\" %s AddMapLegacy"
          Client._key Client._value
          (Escaping.escape_obj obj.DT.name)
          (Escaping.escape_id full_name)
          Client._self
      | FromField(Add, { DT.ty = DT.Set(_); full_name = full_name }) ->
        Printf.sprintf "DB.process_structured_field __t (%s,\"\") \"%s\" \"%s\" %s AddSet"
          Client._value
          (Escaping.escape_obj obj.DT.name)
          (Escaping.escape_id full_name)
          Client._self
      | FromField(Remove, { DT.ty = DT.Map(_, _); full_name = full_name }) ->
        Printf.sprintf "DB.process_structured_field __t (%s,\"\") \"%s\" \"%s\" %s RemoveMap"
          Client._key
          (Escaping.escape_obj obj.DT.name)
          (Escaping.escape_id full_name)
          Client._self
      | FromField(Remove, { DT.ty = DT.Set(_); full_name = full_name }) ->
        Printf.sprintf "DB.process_structured_field __t (%s,\"\") \"%s\" \"%s\" %s RemoveSet"
          Client._value
          (Escaping.escape_obj obj.DT.name)
          (Escaping.escape_id full_name)
          Client._self

      | FromField((Add | Remove), _) -> failwith "Cannot generate db add/remove for non sets and maps"

      | FromObject(Delete) ->
        (Printf.sprintf "DB.delete_row __t \"%s\" %s"
           (Escaping.escape_obj obj.DT.name) Client._self)
      | FromObject(Make) ->
        let fields = List.filter field_in_this_table (DU.fields_of_obj obj) in
        (*	  let fields = db_fields_of_obj obj in *)
        let kvs = List.map (fun fld ->
            Escaping.escape_id fld.full_name,
            OU.ocaml_of_record_field fld.full_name) fields  in
        let kvs' = List.map (fun (sql, o) ->
            Printf.sprintf "(\"%s\", %s)" sql o) kvs in
        Printf.sprintf "DB.create_row __t \"%s\" [ %s ] ref"
          (Escaping.escape_obj obj.DT.name)
          (String.concat "; " kvs')
      | FromObject(GetByUuid) ->
        begin match x.msg_params, x.msg_result with
          | [ {param_type=ty; param_name=name} ], Some (result_ty, _) ->
            let query = Printf.sprintf "DB.db_get_by_uuid __t \"%s\" %s"
                (Escaping.escape_obj obj.DT.name)
                (OU.escape name) in
            _string_to_dm ^ "." ^ (OU.alias_of_ty result_ty) ^ " (" ^ query ^ ")"
          | _ -> failwith "GetByUuid call should have only one parameter and a result!"
        end
      | FromObject(GetByLabel) ->
        begin match x.msg_params, x.msg_result with
          | [ {param_type=ty; param_name=name} ], Some (Set result_ty, _) ->
            let query = Printf.sprintf "DB.db_get_by_name_label __t \"%s\" %s"
                (Escaping.escape_obj obj.DT.name)
                (OU.escape name) in
            if DU.obj_has_get_by_name_label obj
            then "List.map " ^ _string_to_dm ^ "." ^ (OU.alias_of_ty result_ty) ^ " (" ^ query ^ ")"
            else "failwith \\\"Object has no label field\\\""
          | _ -> failwith "GetByLabel call should have only one parameter and a result!"
        end
      | FromObject(GetRecord) -> get_record obj "get_record'"
      | FromObject(Private(GetDBRecord)) -> get_record obj "get_record_internal'"
      | FromObject(Private(GetDBAll)) ->
        (* | FromObject(GetAll) -> *)
        (* Generate the same code for the internal GetDBAll as well as the public GetAll.
           	     Eventually we'll need to provide user filtering for the public version *)
        begin match x.msg_result with
          | Some (Set result_ty, _) ->
            let query = Printf.sprintf "DB.read_refs __t \"%s\""
                (Escaping.escape_obj obj.DT.name) in
            "List.map " ^ _string_to_dm ^ "." ^ (OU.alias_of_ty result_ty) ^ "(" ^ query ^ ")"
          | _ -> failwith "GetAll call needs a result type"
        end
      | FromObject(GetAllRecords) ->
        String.concat "\n"
          [ "let expr' = Db_filter_types.True in";
            "get_records_where ~" ^ Gen_common.context ^ " ~expr:expr'" ]
      | FromObject(GetAllRecordsWhere) ->
        String.concat "\n"
          [ "let expr' = Db_filter.expr_of_string expr in";
            "get_records_where ~" ^ Gen_common.context ^ " ~expr:expr'" ]

     (*
      | FromObject(Private(Copy)) ->
	  begin match x.msg_params with
	  | [ _, src_name, _; _, dst_name, _ ] -> make_shallow_copy api obj (OU.escape src_name) (OU.escape dst_name) (DU.fields_of_obj obj)
	  | _ -> failwith "Copy needs a single parameter"
	  end
	    *)
      | _ -> assert false
    in
    O.Let.make
      ~name: x.msg_name
      ~params: (Gen_common.context_arg :: args)
      ~ty: "'a"
      ~body: (List.map to_string args @ [ open_db_module; body ]) () in

  let obj (obj: obj) =
    let others =
      [ get_record_aux_fn obj;
        get_record_internal_aux_fn obj;
        get_refs_where obj;
        get_records_where obj "get_internal_records_where" "get_record_internal'";
        get_records_where obj "get_records_where" "get_record'";
      ] in
    let bindings = List.map (operation obj) obj.messages @ others in

    let fields = List.map (fun x -> O.Module.Let x) bindings in
    O.Module.make
      ~name:(OU.ocaml_of_obj_name obj.DT.name)
      ~elements:fields
      ~letrec:true
      () in

  let obj_init (obj: obj) =
    O.Module.make
      ~name:(OU.ocaml_of_obj_name obj.DT.name ^ "_init")
      ~elements:(if obj.DT.in_database then [O.Module.Let (register_get_record obj)] else [])
      () in

  let all = Dm_api.objects_of_api api in
  let modules = List.concat (List.map (fun x -> [ obj x; obj_init x]) all) in

  O.Module.make
    ~name:_db_action
    ~preamble:[
      "open Db_cache_types";
      "module D=Debug.Make(struct let name=\"db\" end)";
      "open D";
    ]
    ~elements:(List.map (fun x -> O.Module.Module x) modules) ()

