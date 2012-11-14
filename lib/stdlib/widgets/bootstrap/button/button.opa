import-plugin bootstrap-button

/**
 * Example:
 *
 * import stdlib.themes.bootstrap
 * import stdlib.widgets.bootstrap.button
 *
 * function page() {
 *
 *   <button id=#bt1 type="button" class="btn btn-primary" data-loading-text="Loading...">Loading state</button>
 *   <button id=#bt2 type="button" class="btn" data-toggle="button">Single Toggle</button>
 *   <div class="btn-group" data-toggle="buttons-checkbox">
 *     <button type="button" class="btn">Left</button>
 *     <button type="button" class="btn">Middle</button>
 *     <button type="button" class="btn">Right</button>
 *   </div>
 *   <div class="btn-group" data-toggle="buttons-radio">
 *     <button type="button" class="btn">Left</button>
 *     <button type="button" class="btn">Middle</button>
 *     <button type="button" class="btn">Right</button>
 *   </div>
 *   <button id=#bt3 type="button" class="btn" data-complete-text="finished!" >...</button>
 *
 *   <div onclick={function (_) Button.toggle(#bt2)}>Toggle</div>
 *   <div onclick={function (_) Button.loading(#bt1)}>Loading</div>
 *   <div onclick={function (_) Button.reset(#bt1)}>Reset</div>
 *   <div onclick={function (_) Button.button(#bt3, "complete")}>Complete</div>
 *
 * }
 *
 * Server.start(Server.http, { title: "Button", ~page })
 *
 */
@client
Button = {{

  init(dom:dom) =
    (%%button.init%%)(Dom.to_string(dom))

  toggle(dom:dom) =
    (%%button.toggle%%)(Dom.to_string(dom))

  loading(dom:dom) =
    (%%button.loading%%)(Dom.to_string(dom))

  reset(dom:dom) =
    (%%button.reset%%)(Dom.to_string(dom))

  button(dom:dom, s:string) =
    (%%button.button%%)(Dom.to_string(dom), s)

}}
