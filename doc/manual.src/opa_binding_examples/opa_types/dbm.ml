(**
   1)
   Minimal Opa Binding for Dbm
*)

##extern-type dbm = Dbm.t

(**
   Open a dbm database given its name.
   Return None in case of an error
*)
##register open_db : string -> option(dbm)
let open_db name =
  let flags = [ Dbm.Dbm_rdwr ; Dbm.Dbm_create ] in
  let permission = 0o644 in
  try
    Some (Dbm.opendbm name flags permission)
  with
  | _ -> None

(**
   Close the database
*)
##register close_db \ ` Dbm.close` : dbm -> void

(**
   Find
   Option for opa
*)
##register find : dbm, string -> option(string)
let find dbm key =
  try
    Some (Dbm.find dbm key)
  with
  | Not_found -> None

(**
   Add or replace
*)
##register add \ `Dbm.replace` : dbm, string, string -> void

(**
   2)
   Specialization of the Dbm database
   for a tiny 3 fields database
*)

(**
   We will store in the database a list of people, indexed by their name
   The type [person] is defined in opa, and we will manipulate it there
   using an [opa-type] construction, with the [ServerLib]
*)
##opa-type person

(**
   We will build a representation in Ocaml
*)
type person = {
  name : string ;
  age : int ;
  email : string ;
}

##extern-type ocaml_person = person

(**
   Implementing a traduction between opa and ocaml structures
*)

(**
   We should define the field of the structure
*)
let field_name = ServerLib.static_field_of_name "name"
let field_age = ServerLib.static_field_of_name "age"
let field_email = ServerLib.static_field_of_name "email"

(**
   From Ocaml to Opa
*)
##register opa_person_of_ocaml_person : ocaml_person -> opa[person]
let opa_person_of_ocaml_person ocaml_person =
  let { name ; age ; email } = ocaml_person in
  let opa_person = List.fold_left
    (fun acc (field, value) ->
       ServerLib.add_field acc field value)
    ServerLib.empty_record_constructor
    [
      field_name, Obj.repr (ServerLib.wrap_string name) ;
      field_age, Obj.repr (ServerLib.wrap_int age) ;
      field_email, Obj.repr (ServerLib.wrap_string email) ;
    ]
  in
  wrap_opa_person (ServerLib.make_record opa_person)

(**
   From Opa to Ocaml
*)
##register ocaml_person_of_opa_person : opa[person] -> ocaml_person
let ocaml_person_of_opa_person opa_person =
  let record = unwrap_opa_person opa_person in
  let name = ServerLib.unsafe_dot record field_name in
  let age = ServerLib.unsafe_dot record field_age in
  let email = ServerLib.unsafe_dot record field_email in
  {
    name = ServerLib.unwrap_string name ;
    age = ServerLib.unwrap_int age ;
    email = ServerLib.unwrap_string email ;
  }

(**
   Add a person in a database, printing some logs on the server side
   For the simplicity of this tutorial, we use a marshaling of the
   ocaml structure.
*)
##register add_person : dbm, opa[person] -> void
let add_person dbm person =
  let ocaml_person = ocaml_person_of_opa_person person in
  Printf.eprintf "adding %S/%d/%S in the database\n%!"
    ocaml_person.name
    ocaml_person.age
    ocaml_person.email
  ;
  Dbm.replace dbm ocaml_person.name (Marshal.to_string ocaml_person [])

(**
   Find a person in a database
*)
##register find_person : dbm, string -> option(opa[person])
let find_person dbm name =
  match find dbm name with
  | None -> None
  | Some ocaml_person ->
      let ocaml_person = Marshal.from_string ocaml_person 0 in
      Some (opa_person_of_ocaml_person ocaml_person)
