/**
 * Copyright © 2015 MLstate
 *
 * This file is part of Opa.
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this
 * software and associated documentation files (the "Software"), to deal in the Software
 * without restriction, including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software, and to permit
 * persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
 * FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

/**
 * A language is associated to each user (cookie defined), each time a page is generated
 * in destination to the user it get localised with the current associated language (using
 * ServerI18n.page_lang). The user can change its langage association (using I18n.set_lang).
 * The first time a user connect the association is initialised using Accept-language header
 * field (using ServerI18n.request_lang)
 *
 * An initial language is associatd to the server using system specific environment var
 * (using ServerI18n.get_system_lang). Currenlty the language can't be updated later and
 * can be consulted (using ServerI18n.get_server_lang)
 */

/* @author Henri Chataing */

import-plugin {unix, server}
import stdlib.core.parser
import stdlib.core.map
import stdlib.core.date
import stdlib.core.web.context
import stdlib.core.web.request
import stdlib.core.rpc.core // Ensure dependencies for @sliced_expr are met.

@opacapi Intl_locale = Intl.locale
@opacapi Intl_format = Intl.format
@opacapi Intl_printer = Intl.printer


/** Language selection. */
type Intl.locale = {
  string language,
  string region // Empty if no region, e.g. 'en'
}

type Intl.translation = {
  Intl.printer printer,
  string translation
}

type Intl.printer = external
type Intl.translationMap = Hashtbl.t(string, Intl.translation)

module Intl {

  /** {1} Utils. */

  @stringifier(Intl.locale) both function string localeToString(Intl.locale locale) {
    if (locale.region == "") locale.language else "{locale.language}-{locale.region}"
  }


  /** {1} Intl binding. */

  /** Build a message printer using a set locale and a message format. */
  exposed function Intl.printer serverPrinter(string format, Intl.locale locale) {
    translate(format, locale).printer // Reuse the pre-defined printer.
  }
  client function Intl.printer clientPrinter(string format, Intl.locale locale) {
    translation = translateString(format, locale) // Get the message translation from the server.
    %%IntlClient.printer%%(translation, "{locale}") // Build the message format locally.
  }
  function Intl.printer printer(string format, Intl.locale locale) {
    @sliced_expr({
      client: clientPrinter(format, locale),
      server: serverPrinter(format, locale)
    })
  }

  /** Apply a prepared printer to a context. */
  exposed function string serverFormat(Intl.printer printer, list(string) context) {
    %%IntlServer.format%%(printer, context)
  }
  client function string clientFormat(Intl.printer printer, list(string) context) {
    %%IntlClient.format%%(printer, context)
  }
  function string format(Intl.printer printer, list(string) context) {
    @sliced_expr({
      client: clientFormat(printer, context),
      server: serverFormat(printer, context)
    })
  }


  /** {1} Locale library. */

  /**
   * List locale include directories. Directories can be added to this list using the
   * function 'includeLocaleDirectory'.
   */
  protected localeDirectories = Mutable.make(list(string) [])

  /**
   * Include a locale directory. Note that the directory will be rejected if non existent.
   * The function returns false in such cases.
   */
  function includeLocaleDirectory(string dir) {
    dir = if (String.has_suffix("/", dir)) dir else "{dir}/"
    if (%%BslFile.exists%%(dir)) {
      directories = [dir|localeDirectories.get()] |> List.unique_list_of
      localeDirectories.set(directories)
      true
    } else false
  }

  /**
   * Explore locales directories in order to find the file matching the requested locale.
   * TODO: exploration is not recursive for the moment.
   */
  protected function findLocaleFile(Intl.locale locale) {
    directories = localeDirectories.get()
    localefile = "{locale}.json"
    List.fold(function (dir, file) {
      match (file) {
        case {some: file}: some(file)
        default:
          if (%%BslFile.exists%%("{dir}{localefile}")) some("{dir}{localefile}")
          else none
      }
    }, directories, none)
  }

  /** Return an existing list of existing locale files. */
  protected function listLocales() {
    List.fold(function (dir, locales) {
      if (%%BslFile.exists%%(dir)) {
        (err, files) = %%BslFile.readdir%%(dir)
        if (err == "")
          LowLevelArray.fold(function (file, locales) {
            if (%%BslFile.extname%%(file) == ".json")
              match (parseLocale(%%BslFile.basename%%(file, some(".json")))) {
                case {some: locale}: [locale|locales]
                default: locales
              }
            else locales
          }, files, locales)
        else locales
      } else locales
    }, localeDirectories.get(), []) |>
    List.unique_list_of
  }

  /**
   * Read a locale file and return the contents in a stringmap associating strings to
   * (translated) icu message formats. The structure of the Json must be:
   *
   *  { "string0": "translation0",
   *    "string1": "translation1",
   *    .. }
   */
  protected function importLocale(string localeFile, Intl.locale locale) {
    t0 = Date.now()
    content = %%BslFile.content%%(localeFile) |> Binary.to_string
    translations = match (Json.deserialize(content)) {
      case {some: {Record: strings}}:
        size = List.length(strings)
        translations = Hashtbl.create(size)
        List.iter(function ((string, translation)) {
          match (translation) {
            case {String: translation}:
              printer = %%IntlServer.printer%%(translation, "{locale}")
              Hashtbl.add(translations, string, ~{translation, printer})
            default:
              Log.notice(
                "[Intl]", "While parsing locale file {localeFile}: Unable to parse translation of [{string}]")
          }
        }, strings)
        translations
      default:
        Log.notice("[Intl]", "Unable to parse locale file {localeFile}")
        Hashtbl.create(32)
    }
    t1 = Date.now()
    Log.notice("[Intl]", "importLocale: parsed {localeFile} ({Duration.between(t0, t1) |> Duration.in_milliseconds}ms)")
    translations
  }

  /**
   * Maintain a map of all currently available translation files.
   * Use loadLocale to load in a new locale file (which will be created if non
   * existent).
   */
  protected loadedLocales = Mutable.make(stringmap(Intl.translationMap) StringMap.empty)

  /**
   * Read a locale file, parse it and import the translations into the loadedLocales map.
   * @return the new translation map.
   */
  protected function loadLocale(Intl.locale locale) {
    translations = match (findLocaleFile(locale)) {
      case {some: localeFile}: importLocale(localeFile, locale)
      default: Hashtbl.create(32) // No locale file: import empty stringmap.
    }
    loadedLocales.set(StringMap.add("{locale}", translations, loadedLocales.get()))
    translations
  }

  /**
   * Translate the string for the given locale. The function first checks the loaded locales
   * for the right locale translation map. If the map hans't been loaded, will look through
   * locale directories for the translation file and dynamically load it.
   * If the translation still cannot be found, the translation defaults to the original string,
   * and a binding is added to the translation map with the default value.
   *
   * TODO should export the updated translation file.
   */
  protected function Intl.translation translate(string message, Intl.locale locale) {
    localeStr = "{locale}"
    translationMap =
      match (StringMap.get(localeStr, loadedLocales.get())) {
        case {some: map}: map
        default: loadLocale(locale)
      }
    match (Hashtbl.try_find(translationMap, message)) {
      case {some: translation}: translation
      default:
        printer = %%IntlServer.printer%%(message, localeStr)
        translation = ~{translation: message, printer}
        Hashtbl.add(translationMap, message, translation)
        // translationMap = StringMap.add(message, translation, translationMap)
        // loadedLocales.set(StringMap.add(localeStr, translationMap, loadedLocales.get()))
        exportLocale(locale, translationMap)
        translation
    }
  }

  /** Same as translate, but returns only the string translation. */
  exposed function string translateString(string message, Intl.locale locale) {
    translate(message, locale).translation
  }

  /**
   * Reverse operation: export translation strings.
   * If the locale already has an assigned file, the new translation map is exported to this file.
   * Else, the locale is inserted in the first directory (last included). Translated strings are always
   * inserted in lexical order.
   */
  protected @async function exportLocale(Intl.locale locale, Intl.translationMap translations) {
    file = match (findLocaleFile(locale)) {
      case {some: file}: some(file)
      default:
        match (localeDirectories.get()) {
          case [dir|_]: some("{dir}{locale}.json")
          default: none
        }
    }
    match (file) {
      case {some: file}:
        // The size estimate include the punctutation marks on each line.
        pad = String.byte_length("  \"\": \"\",\n")
        translations = Hashtbl.bindings(translations)
        size = LowLevelArray.size(translations)
        sizeEstimate = LowLevelArray.fold(
          function (~{key, value}, size) { size + String.byte_length(key) + String.byte_length(value.translation) + pad },
          translations, String.byte_length("\{\n\}")
        )
        content = Binary.create(sizeEstimate)
        Binary.add_string(content, "\{\n")
        LowLevelArray.iteri(function (i, ~{key, value}) {
          if (i < size-1) Binary.add_string(content, "  \"{key}\": \"{value.translation}\",\n")
          else Binary.add_string(content, "  \"{key}\": \"{value.translation}\"\n")
        }, translations)
        Binary.add_string(content, "\}")
        // Write the content to the file.
        %%BslFile.write%%(file, content)
      default: void
    }
  }


  /** {1} Locale settings. */

  both defaultLocale = {language: "en", region: "UK"}

  /** Return the locale of the server. */
  exposed function Intl.locale systemLocale() {
    function extract(string key) {
      Option.bind(parseLocale, %%BslSys.get_env_var%%(key))
    }
    // OS specific. TODO Mac and Windows case.
    extract("LANG") ?     // Linux, e.g. fr_FR.UTF-8
    extract("LANGUAGE") ? // Linux, e.g. fr_FR:en
    defaultLocale
  }

  /** Parse a locale string. */
  both function option(Intl.locale) parseLocale(string s) {
    // Will cause problem with UTF-8 on linux LANG.
    sep = parser { case [-_:.,;'`]: void }
    localePart = parser { case part=((!sep .)+) sep*: Text.to_string(part) }
    localeParts = parser { case parts=localePart*: parts }

    match (Parser.parse(localeParts, s)) {
      case [language,region|_]: some(~{language, region})
      default: none
    }
  }

  /** Keeps track of the client locales. */
  private protected userLocale = UserContext.make(option(Intl.locale) none)

  /**
   * Change the client locale.
   * TODO: bind lang observers to propagate lang updates.
   */
  exposed function setLocale(Intl.locale locale) {
    match (thread_context().key) {
      case {client: _}:
        UserContext.change(function (_) { some(locale) }, userLocale)
      default: void
    }
  }

  /** Return the optional locale associated with the active client user. */
  function localeOpt() {
    match (thread_context().key) {
      case {client: _}: UserContext.execute(identity, userLocale)
      default: none
    }
  }

  /**
   * Return the current locale. Depending on the execution side:
   *  - client: returns the client locale.
   *  - server with client: returns the content of the Accept-Locale header.
   *  - on an isolated server: returns the server locale.
   */
  exposed function Intl.locale locale() {
    localeOpt() ? systemLocale()
  }

} // END INTL
