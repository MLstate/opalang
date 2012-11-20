/** @externType dom_element */
/** @externType xhtml */
/** @opaType Cursor.t('a) */

/**
 *  @register { ( -> string ) -> dom_element }
 */
function render(htmlFunc) {
  return Spark.render(htmlFunc);
}

/**
 * @register { ( -> string ) -> string }
 */
function isolate(htmlFunc) {
  return Spark.isolate(htmlFunc);
}

/**
 * @register { string, ( -> string) -> string }
 */
function labelBranch(id, htmlFunc) {
  return Spark.labelBranch(id, htmlFunc);
}

/**
 * @register { Cursor.t('a), ('a -> string), ( -> string) -> string }
 */
function list(cursor, itemFunc, elseFunc) {
  return Spark.list(cursor, itemFunc, elseFunc);
}

/**
 * @register {dom_element, (->dom_element) -> dom_element}
 */
function replace_f(to, f) {
    return to.replaceWith(f);
}
