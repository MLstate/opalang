import-plugin bootstrap-modal

type Modal.options = {
  backdrop: bool
  static: bool
  keyboard: bool
  show: bool
}

/**
 * {2 Example:}
 *
 * {[
 *
 * import stdlib.themes.bootstrap
 * import stdlib.widgets.bootstrap.modal
 *
 * function page() {
 *
 *   Modal.make("md1",
 *     <>Modal header</>,
 *     <p>One fine body…</p>,
 *     <a href="#" class="btn">Close</a>
 *     <a href="#" class="btn btn-primary">Save changes</a>,
 *     Modal.default_options) <+>
 *   <a href="#md1" role="button" class="btn" data-toggle="modal">Launch demo modal</a>
 *
 *   <div onclick={function (_) Modal.show(#md1)}>Show</div>
 *   <div onclick={function (_) Modal.hide(#md1)}>Hide</div>
 *   <div onclick={function (_) Modal.toggle(#md1)}>Toggle</div>
 *
 * }
 *
 * Server.start(Server.http, { title: "Modal", ~page })
 *
 * }
 *
 */
@client
Modal = {{

  @both
  default_options= {
    backdrop = true
    static = false
    keyboard = true
    show = false
  } : Modal.options

  @both
  make(id:string, title:xhtml, body:xhtml, footer:xhtml, options:Modal.options) =
    <div id="{id}" class="modal hide fade"
         role="dialog"
         aria-labelledby="{title}" aria-hidden="true"
         onready={_->init(#{id}, options)}>
      <div class="modal-header">
        <a class="close" data-dismiss="modal" aria-hidden="true">&times;</a>
        <h3>{title}</h3>
      </div>
      <div class="modal-body">
        {body}
      </div>
      <div class="modal-footer">
        {footer}
      </div>
    </div>

  init(dom:dom, options:Modal.options) =
    (%%modal.init%%)(Dom.to_string(dom), options.backdrop, options.static, options.keyboard, options.show)

  toggle(dom:dom) =
    (%%modal.toggle%%)(Dom.to_string(dom))

  show(dom:dom) =
    (%%modal.show%%)(Dom.to_string(dom))

  hide(dom:dom) =
    (%%modal.hide%%)(Dom.to_string(dom))

}}
