type obj = W_Algebra.simple_type_desc
type dir_info = QmlAst.qml_directive * Annot.label
type env_info = Ident.t * Annot.label
type exp_info = Annot.label


type info =
  | FromEnv  of env_info
  | Directive of dir_info
  | Location of Annot.label
  | NoInfo   of string
  | Link     of obj
  | Exception


val cmp_info : info -> info -> int

val add_linked_object : obj -> obj -> unit
val add_loc_object : obj -> Annot.label -> unit
val add_expn_object : obj -> unit
val add_env_object : obj -> env_info -> unit
val add_dir_object : obj -> dir_info -> unit
val addrec_loc_object : obj -> Annot.label -> unit
val addrec_linked_object : obj -> obj -> unit
val addrec_dir_object : obj -> dir_info -> unit
val addrec_env_object : obj -> env_info -> unit
val addrec_expn_object : obj -> unit
val add_no_object : obj -> string -> unit


val clean_type_info : unit -> unit

val retrieve : obj -> info
