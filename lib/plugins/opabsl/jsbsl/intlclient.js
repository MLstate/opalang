/** @opaType Intl.printer */
/** @externType Intl.Client.translationMap */


/** @side {client} */

/**
 * @register {string, string -> Intl.printer}
 */
function printer(format, locale) {
  return new IntlMessageFormat(format, locale);
}


function buildContext(context) {
  var xs = context, k = 0, ctx = {};
  while (xs && xs.hd) {
    ctx['k'+k] = xs.hd;
    k++; xs = xs.tl;
  }
  return ctx;
}

/**
 * @register {Intl.printer, opa[list(string)] -> string}
 */
function format(printer, context) {
  var ctx = buildContext(context);
  return printer.format(ctx);
}

var loadedTranslations = [{none: {}}];

/**
 * @register {-> Intl.Client.translationMap}
 */
function getTranslations() {
  return loadedTranslations[0];
}

/**
 * @register {Intl.Client.translationMap -> void}
 */
function setTranslations(map) {
  loadedTranslations[0] = map;
}
