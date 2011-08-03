/**
 * Simple server using big int
**/

/**
 * {1 Binding}
**/

/**
 * The type definition in opa.
 * The name should be the same as the one used in the plugin
 * The keywork 'external' means that the implementation is not
 * available in Opa, and values of this type can only be manipulated
 * via the API of the plugin.
**/
type BigInt.t = external

BigInt = {{

  /**
   * The syntax for binding foreign functions is %% key %%
  **/
  add = %% bigint.add %%
  to_string = %% bigint.to_string %%

  /**
   * For lisibility of the code, we can add types coercion
  **/
  of_string = %% bigint.of_string %% : string -> option(BigInt.t)

}}

/**
 * {1 Web page}
**/

/**
 * Compute the addition, or return a hint message in case of error
**/
@publish compute(a, b) =
  match (BigInt.of_string(a), BigInt.of_string(b)) with
  | ( { some = i_a }, { some = i_b } ) ->
    i_add = BigInt.add(i_a, i_b)
    BigInt.to_string(i_add)
  | ( { none }, _) -> "\"{a}\" is not a valid big int"
  | _ -> "\"{b}\" is not a valid big int"

/**
 * Action when the user will click on the 'add' button.
 * get the values of the 2 inputs, and store the result
 * in the result box
**/
action() =
  a = Dom.get_value(#input_a)
  b = Dom.get_value(#input_b)
  Dom.transform([ #result <- compute(a, b) ])

button(id, message) =
  <a id={id:string}
     class="button"
     ref="#"
     onclick={_->action()}>{message:string}
  </a>

page() =
  <>
  <h1>Big int Binding</h1>
  <h2>Arguments</h2>
  <input style="width:100%" id="input_a"/><br/>
  <input style="width:100%" id="input_b"/>
  <h2>Addition (on the server, using the external implementation)</h2>
  <div id="result" />
  <br/>
  {button("addition", "add")}<br/>
  </>

server = one_page_server("Big int Binding", page)
