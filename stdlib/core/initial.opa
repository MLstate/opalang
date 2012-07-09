/*
    Copyright © 2011, 2012 MLstate

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
 * Basic definitions of common types and values.
 *
 * These definitions are accessible by all programs and are used pervasively by the standard library itself.
 *
 * @author David-Rajchenbach-Teller, 2010 (documentation)
 * @destination public
 * @stability stable
 * @category standard
 */


/**
 * {1 Elementary types}
 */

/**
 * {2 Booleans}
 *
 * Booleans represent truth values. There are exactly two booleans: [{false}] and [{true}]. Booleans
 * are used pervasively in OPA, in particular for control flow. Booleans can be manipulated using
 * [if...then...else...] or pattern-matching.
 *
 * More functions on booleans are defined in module [Bool].
 */

@opacapi
type bool = {false} / {true}

/**
 * A shortcut for [{true}].
 */
true ={true}:bool

/**
 * A shortcut for [{false}].
 */
false={false}:bool

/**
 * Boolean negation
 */
not(b) = if b then false else true

/**
 * {2 Void}
 */
/**
 * The value of nothing.
 */
@opacapi
type void = {}
void={}:void

/**
 * {1 Comparison functions}
 *
 * All comparison functions return a boolean.
 */


/**
 * The basic monomorphic comparison functions
 */
compare_int = Int.compare
compare_string = String.compare
compare_float = Float.compare


/**
 * The structural polymorphic comparison
 */
@specialize(compare_int,compare_string,compare_float)
compare(a,b) = OpaValue.compare(a,b)

/**
 * Structural equality.
 *
 * [a == b] returns [true] if and only if [a] and [b] are structurally equal.
 * For instance, ["blue" == "blue"] or [{field = "blue"} == {field = "blue"}]
 * will return [true].
 *
 * Note that structural equality does not work on functions.
 */
@opacapi
@specialize(String.equals,Int.equals,Float.equals)
`==`(a:'a,b:'a):bool = match compare(a,b) with  {eq} -> true  | _ -> false

/**
 * Universal comparison functions
 */

@specialize(String.`lt`,Int.`<`,Float.`<`)
`<`(a:'a,b:'a)  = match compare(a,b) with {lt} -> true  | _ -> false

@specialize(String.`gt`,Int.`>`,Float.`>`)
`>`(a:'a,b:'a)  = match compare(a,b) with {gt} -> true  | _ -> false

@opacapi
@specialize(String.`ne`,Int.`!=`,Float.`!=`)
`!=`(a:'a,b:'a) = match compare(a,b) with {eq} -> false | _ -> true

@specialize(String.`ge`,Int.`>=`,Float.`>=`)
`>=`(a:'a,b:'a) = match compare(a,b) with {lt} | {neq} -> false | _ -> true

@specialize(String.`le`,Int.`<=`,Float.`<=`)
`<=`(a:'a,b:'a) = match compare(a,b) with {gt} | {neq} -> false | _ -> true

@specialize(Int.min,Float.min)
min(a:'a,b:'a)  = match compare(a,b) with {lt} -> a     | _ -> b

@specialize(Int.max,Float.max)
max(a:'a,b:'a)  = match compare(a,b) with {gt} -> a     | _ -> b


eq(a,b)= a==b

/**
 * Physical equality.
 *
 * This operator is for advanced uses, as it compares memory addresses of two objects.
 * Unless you know what you are doing, chances are that you should rather use operator [`==`].
 *
 * [a===b] produces [true] if and only if [a] and [b] are the same object -- not just if
 * they are equal. For instance, [a = {blue}; a === a] returns [true], while
 * ["blue" === "blue"] will probably return [false]. Note that circumstances beyond the control
 * of the developer may alter the physical equality of two objects: compiler optimizations may
 * transform two immutable identical objects into one, while concurrency or distribution may
 * cause an object to be duplicated transparently.
 */
`===` = %% Bslpervasives.areSameObject %%  : 'a, 'a -> bool


/**
 * {1 Elementary combinators}
 */

/**
  A function that discards its argument
*/
ignore(_:'a) : void = void

/**
 * The identity function
 *
 * For every [x], [identity(x)] is equal to [x]
 */
@opacapi identity(x:'a) = x : 'a

/**
 * The pipe.
 *
 * The pipe is a reversed composition operator. That is, [a |> f] is equivalent to [f(a)].
 * Use this operator to chain function calls. For instance, [5 |> _ + 10 |> _ + 20] produces
 * result [35].
 *
 * Use it to chain function calls.
 */
// CAUTION, |> is currently inlined by the compiler using `|>`(a,f) = f(a)
`|>`(a,f) = f(a)

/**
 * Composition
 *
 * This is the forward composition operator. That is, [f@g] is equivalent to [x -> f(g(x))].
 * Use this operator to compose functions for a later call. For instance, [_ + 10 @ _ + 20]
 * is the function [x -> x + 30].
 */
`@`(f,g) =
    new_f(x)=f(g(x))
    new_f

`@>`(g, f) = f @ g

compose = `@`
reverse_compose = `@>`



/**
 * {1 Numbers}
 */


/**
 * {2 Operations on integers}
 *
 * More operations on integers are defined in module [Int].
 */

/**
 * A shortcut for [_ + 1]
 */
succ(x : int) = x + 1

/**
 * A shortcut for [_ - 1]
 */
pred(x : int) = x - 1

/**
 * Arithmetic operations
 *
 */

unary_minus_int = %% Bslpervasives.int_neg %%

mod = %% Bslpervasives.int_mod %%
rem = %% Bslpervasives.int_rem %%

/**
 * Magic overloaded operator '+'
 * This operator is defined only on [int], [float], [string]
 * The overloading of this operator is resolved at compile time, which means that
 * if the context does not give enough type information for inferring one of the supported type,
 * this will raise an error during the compilation, inviting the user to add a type annotation.
**/
@specialize_strict(Int.`+`, Float.`+`,String.`^`)
`+`(_ : 'number, _ : 'number) : 'number = @fail

/**
 * Magic operator '-'
 * Cf documentation of {!+}.
 * Defined only on [int] and [float]
**/
@specialize_strict(Int.`-`, Float.`-`)
`-`(_ : 'number, _ : 'number) : 'number = @fail

/**
 * Magic operator '-'
 * Cf documentation of '+'.
 * Defined only on [int] and [float]
**/
@specialize_strict(Int.`*`, Float.`*`)
`*`(_ : 'number, _ : 'number) : 'number = @fail

/**
 * Magic operator '-'
 * Cf documentation of '+'.
 * Defined only on [int] and [float]
**/
@specialize_strict(Int.`/`, Float.`/`)
`/`(_ : 'number, _ : 'number) : 'number = @fail

/**
 * {2 Operations on floating-point numbers}
 *
 * More operations on floating-point numbers are defined in module [Float].
 */

@deprecated({hint="use undotted operator"}) `+.` = Float.`+`
@deprecated({hint="use undotted operator"}) `-.` = Float.`-`
@deprecated({hint="use undotted operator"}) `*.` = Float.`*`
@deprecated({hint="use undotted operator"}) `/.` = Float.`/`

/**
 * Arithmetic operations
 *
 * The following functions are equivalent to arithmetic operators. They are provided essentially to simplify the life of developers who
 * wish to generate OPA code from specification languages.
 */

/**
 * Still introduced by the parser. Should be removed before the release,
 * once applications are patched, and do not use the [-.] syntax.
 * @opacapi: introduced by the parser as resolution of unary [-.]
**/
@opacapi
@deprecated({hint="use undotted operator"})
unary_minus_dot   = %% Bslpervasives.float_neg %% : float -> float


/**
 * Magic overloaded operator unary '-'
 * This operator is defined only on [int], [float]
 * @opacapi: introduced by the parser as resolution of unary [-]
**/
@opacapi
@specialize_strict(unary_minus_int, %% Bslpervasives.float_neg %%)
unary_minus(_:'number) : 'number = @fail

/**
 * {1 Debugging utilities}
 *
 * More utilities are provided in module [Debug].
 */

/**
 * This is just an inline of the directive [@fail],
 * which will report the position of the error in the source code.
**/
@expand
error(s) = @fail(s)

/**
 * Print a message and continue.
 *
 * If this function is executed on the client, the message will be displayed in a side window.
 * On the server, the message will be displayed on the console.
 */
jlog = %% Bslpervasives.jlog %%


`^`  = %% BslString.concat %%

/**
 * A shortcut for ^
 */
sc = `^`

print = %% Bslpervasives.print_string %% : string -> void
prerr = %% Bslpervasives.prerr_string %% : string -> void

/**
 * Same than print, prerr but add a newline char, and flush.
**/
println = %% Bslpervasives.print_endline %% : string -> void
prerrln = %% Bslpervasives.prerr_endline %% : string -> void


print_int = %% Bslpervasives.print_int %% : int -> void

/**
 * {1 Loops}
 *
 * In OPA, loops are functions. If you come from a different language, this may come as a surprise.
 * The most general looping function is called [for]. You can easily define other looping functions
 * from [for] or from more primitive constructions.
 */

/**
 * General loop
 *
 * This function implements a general-purpose looping construction.
 *
 * Example: [for(0, (i -> do jlog("{i}"); i + 1), _ < 50)] will print
 * [0], then [1], then [2], ..., until [49].
 *
 * Alternative syntax: [for(0, _ , _<50)(i -> do jlog("{i}"; i+1))].
 *
 *
 * @param init An initial value.
 * @param next The body of the loop. It takes as argument the latest state of the loop and returns the next state.
 * @param cond A function determining whether the loop should continue. If it returns [true], the loop continues.
 * Otherwise, it stops.
 */
for(init:'state, next:'state -> 'state, cond:'state -> bool) : 'state =
  if cond(init) then
    for(next(init),next,cond)
  else
    init

/**
 * A loop on integers.
 *
 * Example: [inrange(0, 10, do jlog("{_}"))] will print [0], [1], ... until [10]
 *
 * @param first The start of the loop (included).
 * @param last The end of the loop (included).
 * @param action The body of the loop. It takes as argument the latest state of the loop and returns nothing.
 */
inrange(first:int, last:int, action: int -> void) =
  _ = for(first, (i -> do action(i); i+1), _ <= last)
  void

/**
 * Calls a function a given number of times
 *
 * NB: only useful if f has a side-effect
 *
 * @param n The number of times to call f
 * @param f The function to be called n times
 */
repeat(n : int, f : -> void) : void =
  ignore(for(0, (i -> do f(); i+1), _ < n))

/**
 * Calls a function while it returns true, while accumulating a state
 *
 * @param f The function to be called as long as it returns true
 */
while(state:'state, f : 'state -> ('state, bool)) : 'state =
  (state, continue) = f(state)
  if continue then while(state,f) else state

/**
 * An alias for [for]
 */
unfold = for


/**
 *
 * Short cut operators : &&,  ||
 * They have usual lazy usual semantics on first-order call.
 *
 */

@expand
`&&`(a,b) =
  if a then b
  else
    /*
      JS imp backend may simplify pattern matching generated
      from && in opa code. We can easily match the form of
      the generated code if the following bool is syntactic.
      Do not replace it with the identifier [false]
    */
    {false} : bool

@expand
`||`(a,b) =
  if a
  then
    /*
      cf &&, same remark about js optimization
    */
    {true} : bool
  else b

xor(left, right) = if left then not(right) else right

/**
 * {6 Conversion function}
 */

/**
 * The basic monomorphic conversion
 */
string_of_int = String.of_int
int_to_string = string_of_int

string_of_float = String.of_float
float_to_string = string_of_float

//int_of_string             = Parser.int
//int_of_string = %% BslNumber.Int.of_string %%
//string_to_int = int_of_string

int_of_float = Int.of_float // %% BslNumber.Int.of_float %% : float -> int
float_to_int = Int.of_float
float_of_int = Float.of_int
int_to_float = Float.of_int

//float_of_string = Float.of_string
string_to_int = Int.of_string
string_to_float = Float.of_string

string_of_void() = String.of_void
void_to_string = String.of_void
int_of_first_char         = Int.of_utf8

voidToString              = Void.to_string
intToString               = Int.to_string
stringToString            = String.to_string
boolToString              = Bool.to_string
floatToString             = Float.to_string

/**
 * The polymorphic conversion to string.
 * Used for the string insertion syntax, e.g. ["{myOpaValueInAString}"]
 */
@opacapi
@specialize(voidToString,intToString,stringToString,boolToString,floatToString)
magicToString(s) = OpaValue.to_string(s)
