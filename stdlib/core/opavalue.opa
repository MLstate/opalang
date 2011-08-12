/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * Manipulating OPA values and provides some magic functions.
 *
 * @category introspection
 * @destination ??
 * @author Quentin Bourgerie
 * @stability almost stable
 */

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface for module OpaValue}
 */
/* disabled for S3:
type OpaValue.interface = {{
  /* Some magic functions */
  typeof : 'a -> OpaType.ty
  to_string : 'a -> string
  compare : 'a -> 'a -> int

  /* Serialization/Unserialization */
  serialize : 'a -> string
  partial_serialize : 'a -> OpaType.ty -> OpaValue.unser
  finish_serialize : OpaValue.unser -> string

  unserialize : string -> OpaType.ty -> option('a)
  partial_unserialize : string -> option(OpaValue.unser)
  finish_unserialize : OpaValue.unser -> OpaType.ty -> option('a)

  /* Records */
  Record : {{
    /* Loops */
    fold : (Record.field -> 'c -> 'b -> 'b) -> 'a -> 'b -> 'b;
    fold2 : (Record.field -> 'c -> 'c -> 'b -> 'b) -> 'a -> 'a -> 'b -> 'b;

    /* Field translation */
    name_of_field : Record.field -> option(string);
    name_of_field_unsafe : Record.field -> string;

    field_of_name : string -> option(Record.field);
    field_of_name_unsafe : string -> Record.field;

    field_of_type_field : OpaType.Field.label -> option(Record.field);
    field_of_type_field_unsafe : OpaType.Field.label -> Record.field;

    type_field_of_field : Record.field -> option(OpaType.Field.label);
    type_field_of_field_unsafe : Record.field -> OpaType.Field.label;

    /* Record constructor */
    empty_constructor : record_constructor;
    add_field : record_constructor -> Record.field -> 'a -> record_constructor;
    make_record : record_constructor -> 'a;
  }}

  /* Private function - When we have module sig remove that... */
  fields_of_fields_list : 'a -> list(OpaType.fields) -> (OpaType.fields, int)
  fields_of_fields_list2 : list(OpaType.Field.label) -> list(OpaType.fields) -> option(OpaType.fields)

}}
*/

/**
 * {1 Types defined for OpaValue}
 */

/**
 * Type of field of record.
 */
type Record.field = external

/**
 * Type used for construct a record.
 */
type Record.constructor = external

/**
 * Type of closures
 */
type Closure.t = external

/**
 * Type of closures arguments
 */
type Closure.args = external

/**
 * The identifier stored inside the closures
 */
type Closure.identifier('a) =
  { closure_name : string }
/ { closure_name : string; stored : 'a }

OpaValue = {{
  /**
   * Make an error message...
   */
  error_message(fun_name:string, original_ty, ty) =
    "[{fun_name}] on {OpaType.to_pretty(original_ty)} is impossible.\n" ^
    "Because contains a value of type {OpaType.to_pretty(ty)}."

  todo_magic_container(get, ident, args, todo_with_type, alt, value) =
    match get(ident) with
    | {none} -> alt(value)
    | {some = f} ->
      nargs = List.length(args)
      match nargs with
      | 0 -> f(value)
      | _ ->
        clos_arg = OpaValue.Closure.Args.create(nargs + 1)
        do List.iteri(
          (i, ty ->
            OpaValue.Closure.Args.set(clos_arg, i,
              todo_with_type(ty, _))
          ), args)
        do Closure.Args.set(clos_arg, nargs, value)
        OpaValue.Closure.apply(@unsafe_cast(f), clos_arg)


  /**
   * {2 Some magic functions}
   */

  /**
   * Returns the type of given [value]
   */
  typeof(value) = @typeof(value)

  /**
   * Returns a string representation of given [value]
   * The polymorphic conversion to string (for debug for instance)
   * or for display of simple structures. Used for string inserts.
   */
  to_string(value) = to_string_with_type(@typeof(value), value)

  to_string_with_type(ty, value) =
    `++` = `^`
    /* For record and sum, take value and fields */
    rec aux_rec(value, fields, text) =
      if List.is_empty(fields) then text ++ "\{}"
      else
      (Record.fold_with_fields(
        (field, tyfield, value, (text, pre) ->
          name = Record.name_of_field_unsafe(field)
          res = aux(value, tyfield, "")
          ((text ++ pre ++ name ++ " = " ++ res), "; ")
        ), value, fields, (text,"\{")
      )).f1 ++ "}"

    /* Main aux function ************************/
    and aux(value, ty, text) =
      match ty with
      /* Basic case *****************************/
      | {TyConst = {TyInt}} -> Int.to_string(Magic.id(value))
      | {TyConst = {TyFloat}} -> Float.to_string(Magic.id(value))
      | {TyConst = {TyString}} -> Magic.id(value)

      /* Record case ****************************/
      | {TyRecord_row = row}
      | {TyRecord_row = row; TyRecord_rowvar = _} ->
        aux_rec(value, row, text)
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        aux_rec(value, OpaType.fields_of_fields_list(value, col).f1, text)

      /* Encapsuled types ***********************/
      | {TyName_args = args; TyName_ident = ident} ->
        todo_magic_container(
          %%BslValue.MagicContainer.to_string_get%%,
          ident, args, to_string_with_type,
          aux(_, OpaType.type_of_name(ident, args), text),
          value)

      /* Other case *****************************/
      | {TyVar = var} -> text ++ var
      | {TyArrow_params = _ TyArrow_res = _} ->
        text ++ "<a function of type " ++ OpaType.to_pretty(ty) ++ ">"
      | {TyAbstract} ->
        text ++ "<a value of abstract type>"

      | { TyForall_quant=_ ~TyForall_body} ->
        //TODO : ??
        aux(value, TyForall_body, text)
    aux(Magic.id(value), ty, "")


  /**
   * Generic compare. [compare x y] returns [0] if [x == y], [-1] if
   * [x < y] and [1] if [x > y].
   */
  compare(a, b) =
    original_ty = @typeof(a)
    /* For record and sum, take values and fields */
    rec aux_rec(a, b, fields) =
      Record.fold2_with_fields(
        (_field, ty, a, b, acc ->
           match acc with
             | {eq} -> aux(a, b, ty)
             | cmp -> cmp
        ), a, b, fields, {eq}
      )
    /* Main aux function ************************/
    and aux(a, b, ty) =
      match ty with
      /* Basic case *****************************/
      | {TyConst = {TyInt}} -> compare_int(Magic.id(a), Magic.id(b))
      | {TyConst = {TyFloat}} -> compare_float(Magic.id(a), Magic.id(b))
      | {TyConst = {TyString}} -> compare_string(Magic.id(a), Magic.id(b))

      /* Record case ****************************/
      | {TyRecord_row = row}
      | {TyRecord_row = row; TyRecord_rowvar = _} -> aux_rec(a, b, row)
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        /* Perhaps here it's problematics : it depends of the order of sum */
        (fields_a, pa) = OpaType.fields_of_fields_list(a, col)
        (_fields_b, pb) = OpaType.fields_of_fields_list(b, col)
        match Int.compare(pa, pb) with
          | {eq} -> /* fields_a = fields_b in this branch */ aux_rec(a, b, fields_a)
          | cmp  -> cmp
        end

      /* Encapsuled types ***********************/
      | {TyName_args = args; TyName_ident = ident} ->
        match %%BslValue.MagicContainer.compare_get%%(ident) with
        | {none} -> aux(a, b, OpaType.type_of_name(ident, args))
        | {some = comparator} ->
          nargs = List.length(args)
          match nargs with
          | 0 -> comparator(a, b)
          | _ ->
            clos_arg = OpaValue.Closure.Args.create(nargs + 2)
            do List.iteri(
              (i, ty ->
                OpaValue.Closure.Args.set(clos_arg, i, aux(_, _, ty))
              ), args)
            do Closure.Args.set(clos_arg, nargs, a)
            do Closure.Args.set(clos_arg, nargs + 1, b)
            OpaValue.Closure.apply(@unsafe_cast(comparator), clos_arg)
          end
        end

      /* Error case *****************************/
      | {TyForall_quant = _; TyForall_body = _}
      | {TyArrow_params = _; TyArrow_res = _}
      | {TyAbstract}
      | {TyVar = _} ->
        error(error_message("compare", original_ty, ty))
  aux(a, b, original_ty)

  /**
   * {2 Record manipulation}
   * This module provides some functions for manipulate records.
   */
  Record = {{
    /**
     * {3 Dot}
     */
    /**
     * [dot record field] Access to a [field] of a [record].
     * Return [none] if the field is not present.
     */
    dot = %%BslValue.Record.dot%%

    /**
     * [dot record field] Access to a [field] of a [record].
     * Cause a runtime error if the field is not present.
     */
    unsafe_dot = %%BslValue.Record.unsafe_dot%%

    /**
     * {3 Loops on record}
     */
    /**
     * Fold a record. [fold folder record acc] parameter
     * [record] must be a record else behavior is not know (at best
     * you'll get a segfault). [folder] is a function like this :
     * [folder field value_of_field acc].
     */
    fold = %%BslValue.Record.fold_record%%

    /**
     * Fold two record. [fold2 folder record1 record2 acc]
     * parameter [record1] and [record2] must be a record and they
     * must be identical (same structure). Else behavior is not know
     * (at best you'll get a segfault). [folder] is a function like
     * this : [folder field value_of_field1 value_of_field2 acc].
     */
    fold2 = %%BslValue.Record.fold_2_record%%

    /**
     * Fold a record with a list of fields. Like [fold] but folder
     * take an additional type argument and you provide the corresponding
     * [fields]. If [fields] are ordered this functions ensures that
     * folder receive the type corresponding to the value.
     */
    fold_with_fields(folder : Record.field, OpaType.ty, 'a, 'acc -> 'acc, value, all_fields : OpaType.fields, acc : 'acc) =
      fold(
        (field, value, (fields : OpaType.fields, acc : 'acc) ->
          match fields with
          | [hd | tl] -> (tl, folder(field, hd.ty, value, acc))
          | [] -> error("[Record.fold_with_fields] value:"^Debug.dump(value)^" all_field:"^Debug.dump(all_fields)^" fields:"^Debug.dump(fields))),
        value, (all_fields, acc)
      ).f2 : 'acc

    /**
     * A combination of [fold2] and [fold_with_fields].
     */
    fold2_with_fields(folder, a, b, fields : OpaType.fields, acc : 'acc) =
      fold2(
        (field, a, b, (fields : OpaType.fields, acc : 'acc) ->
          match fields with
          | [hd | tl] -> (tl, folder(field, hd.ty, a, b, acc))
          | [] -> error("[Record.fold2_with_fields] ")),
        a, b, (fields, acc)
      ).f2 : 'acc


    get_uniq_field_name(r)=
      match fold(field,_,acc->[field|acc],r,[])
      [field] -> name_of_field(field)
      _ -> none

    /**
     * {3 Field translation}
     * This part provides some functions for make translation with
     * string. And with OpaType.Field.label, indeed representation of
     * field in type, and at runtime is not necessarily same.
     */
    /** {4 Translation with string} */
    /**
     * Get the name of field. If field doesn't exists return [none]
     */
    name_of_field = %%BslValue.Record.name_of_field%%
    /**
     * Get the name of field. If field doesn't exists make an error.
     */
    name_of_field_unsafe(field) =
      Option.lazy_default(-> error("name_of_field_unsafe"),
        Record.name_of_field(field))

    /**
     * Get the field corresponding to a string. If given parameter
     * corresponding to any record return [none].
     */
    field_of_name = %%BslValue.Record.field_of_name%%
    /**
     * Get the field corresponding to a string. If given parameter
     * corresponding to any record make an error.
     */
    field_of_name_unsafe(name) =
      Option.lazy_default(-> error("field_of_name_unsafe : " ^ name),
        Record.field_of_name(name))

    /** {4 Translation with OpaType.Field.label} */
    /**
     * Translate a type field  [OpaType.Field.label] in a real
     * (runtime) field [Record.field].
     */
    field_of_type_field(ft:OpaType.Field.label) =
      field_of_name(ft)
    /**
     * Unsafe field_of_type_field
     */
    field_of_type_field_unsafe(ft:OpaType.Field.label) =
      Option.lazy_default(
        -> error("field_of_type_field_unsafe : Field doesn't exists at runtime"),
        field_of_name(ft))

    /**
     * Translate a real (runtime) field [Record.field] in field
     * [OpaType.Field.label].
     */
    type_field_of_field(rf:Record.field) =
      (match name_of_field(rf) with
      | {some = name} -> some(name)
      | {none} -> {none})
      : option(OpaType.Field.label)
    /**
     * Unsafe type_field_of_field
     */
    type_field_of_field_unsafe(rf:Record.field) =
      Option.lazy_default(
        -> error("type_field_of_field_unsafe : Field doesn't exists at runtime"),
        type_field_of_field(rf))



    /**
     * {3 Record construction}
     */
    /**
     * It's empty (initial) record constructor.
     */
    empty_constructor = %%BslValue.Record.empty_constructor%%

    /**
     * Add field and associated value to a record constructor.
     * WARNING : This method has a functional interface but it can be
     * imperative (it imperative on js)
     */
    add_field = %%BslValue.Record.add_field%%

    /**
     * Make a record from a record constructor.
     */
    make_record = %%BslValue.Record.make_record%%

    /**
     * Make a simple constructor from a field name
     */
    make_simple_record : Record.field -> _ = %% BslValue.Record.make_simple_record %%
  }}

  /**
   * {2 Closure manipulation}
   * This module provides some functions for manipulate closures.
   *
   * {3 Example}
   *
   * An quick example of using the [Closure] module. We create a
   * closure (int, string -> int) from a closure (Closure.args ->
   * int).
   *
   * _ =
   *   /* The anyarray OPA closure */
   *   anyf(args) =
   *     a0 = OpaSerialize.Closure.Args.get(args,0):int
   *     do jlog("argument(0):int = {a0}")
   *     do jlog("argument(1):string = {OpaSerialize.Closure.Args.get(args,1):string}")
   *     a0 + a0
   *
   *   /* Make a closure : arity = 2 */
   *   f = OpaSerialize.Closure.create(anyf, 2, "an_identifier")
   *
   *   /* Apply with Closure.apply */
   *   args = OpaSerialize.Closure.Args.create(2)
   *   do OpaSerialize.Closure.Args.set(args, 0, 1)
   *   do OpaSerialize.Closure.Args.set(args, 1, "Call with Closure.apply")
   *   do jlog("Result of first call : {OpaSerialize.Closure.apply(f, args):int}")
   *
   *   /* Apply after coerce */
   *   g = Magic.id(f) : int, string -> int
   *   do jlog("Result of second call : {g(2,"Call after coerce")}")
   *   void
   *
   */
  Closure = {{

    /**
     * {2 Closure arguments manipulation}
     * The arguments of closure can be represented by a untyped
     * vector, this module provides functions for manipulate these
     * vectors.
     */
    Args = {{

      /**
       * [create n] create an arguments vector of size [n].
       */
      create = %% BslClosure.Args.create %% : int -> Closure.args

      /**
       * Returns size of given arguments vector.
       */
      length = %% BslClosure.Args.length %% : Closure.args -> int

      /**
       * [set args i a] replace the element number [i] by [a] on the
       * arguments vector [args].
       */
      set = %% BslClosure.Args.set %% : Closure.args, int, 'a -> void

      /**
       * [get args i] returns the element number [i] of the arguments
       * vector [args]
       */
      get = %% BslClosure.Args.get %% : Closure.args, int -> 'a

      /**
       * Create a arguments vector from a list.
       */
      of_list(lst) =
        args = Closure.Args.create(List.length(lst))
        do List.iteri(Closure.Args.set(args,_,_), lst)
        args

      /**
       * Returns a untyped list from an arguments vector.
       */
      to_list(args) =
        rec aux(l, i) =
          if i == -1 then
            l
          else
            aux([get(args, i)|l], i - 1)
        aux([], length(args) - 1)
    }}

    /**
     * [create f arity ident] create an OPA closure that take [arity]
     * arguments from an OPA closure that take an arguments vector of
     * size [arity]. The identifier of created closure will be
     * [ident].
     */
    create(f : Closure.args -> 'a, arity : int, ident : string) =
      export = %% BslClosure.export %% : Closure.t -> 'native
      bslcreate = @may_cps(%%BslClosure.create_anyarray%%) : 'native, int, 'ident -> Closure.t
      native = export(Magic.id(f))
      bslcreate(native, arity, {closure_name = ident})
    : Closure.t

    /**
     * [apply f args] Apply the arguments vector [args] to the closure
     * [f].
     */
    apply : Closure.t, Closure.args -> 'a =
     @may_cps(%%BslClosure.apply%%)

    /**
     * [apply_with_ty f args ty_args] Apply the arguments vector [args] to the closure
     * [f].
     */
    env_apply_with_ty : Closure.t, Closure.args, Closure.args -> 'a =
     @may_cps(%%BslClosure.env_apply_with_ty%%)

    /**
     * Returns a boolean that indicates if the environment of a
     * closure is empty.
     */
    is_empty = %%BslClosure.is_empty%%
      : Closure.t -> bool

    /**
     * Store to a [closure] a [value]. You can do it just for fun or for
     * real reasons.
     */
    set_stored(closure, stored) =
      match bslgi(closure) with
      | {~closure_name ...} -> bslsi(closure, {~closure_name ~stored})
      | _ -> // assert false
        error("[OpaSerialize.Closure.set_identifier] Any identifier")

    /**
     * Get the string identifier of a closure.
     */
    get_identifier(closure : Closure.t) =
      match bslgi(closure) with
      | {~closure_name ...} -> closure_name : string
      | _ -> // assert false
        error("[OpaSerialize.Closure.get_identifier] Any identifier")

    /**
     * Get a stored value on a [closure]. A value can be stored on
     * closure with [set_stored]. Returns [none] if any value was
     * stored on closure.
     */
    get_stored(closure) =
      match bslgi(closure) with
      | {closure_name=_ ~stored} -> some(stored)
      | _ -> none


    /**
     * Get the closure environment
     */
    get_args = %% BslClosure.get_args %% : Closure.t -> Closure.args

    /**
     * Get the closure environment type informations
     */
    get_ty_args = %% BslClosure.get_ty_args %% : Closure.t -> Closure.args

    /**
     * Returns the distant identifier of closure if it implementation
     * is present on the other side.
     */
    on_distant(closure) =
      on_distant = %%BslClosure.on_distant%% : string -> bool
      id = get_identifier(closure)
      if on_distant(id) then some(id)
      else none

    /**
     * Returns the local implementation of closure identified by the
     * given string.
     */
    get_local = %%BslClosure.get_local%% : string -> option(Closure.t)

    /**
     * Returns the local identifier of closure if an implementation is
     * present on this side.
     */
    on_local(clos) =
      id = get_identifier(clos)
      Option.map(_ -> id, get_local(id))

    /**
     * Get ll identifier of a closure (should contains a record with at
     * least closure_name fields)
     */
    @private bslgi(closure) : Closure.identifier =
      bslgi : 'closure -> option(Closure.identifier) = %%BslClosure.get_identifier%%
      match bslgi(closure) with
      | {some = x} -> x
      | _ -> // Can ocurs on projection
        {closure_name = ""}

    /**
     * Set ll identifier of a closure
     */
    @private bslsi = %%BslClosure.set_identifier%% : 'closure, Closure.identifier -> void

    //show = %% BslClosure.show %%

  }}

}}

@opacapi OpaValue_add_to_string = %%BslValue.MagicContainer.to_string_add%%

@opacapi OpaValue_add_compare = %%BslValue.MagicContainer.compare_add%%

@opacapi OpaValue_add_serializer = %%BslValue.MagicContainer.serializer_add%%

@opacapi OpaValue_add_xmlizer = %%BslValue.MagicContainer.xmlizer_add%%
