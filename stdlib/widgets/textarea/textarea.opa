/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

/**
 * A textarea with more control over text editing.
 *
 * @category WIDGET
 * @author Guillem Rieu, 2010
 * @destination PUBLIC
 * @stability TRANSITIONAL
 */

// Bugs to fix:
// TODO: '[substring] out of range' when inserting caret (get_text empty)
// TODO: 'Actions empty DOM selection' (when moving between deleted lines?)

// Short term
// TODO: a show function
// TODO: missing key handling: page up / down
// TODO: text selection functions (Shift + arrows, C-A and mouse selections)

// Soon
// TODO: rich text editing
// TODO: line numbering
// TODO: word wrapping (a single real line displayed on multiple HTML lines)
//       (mimic behaviour of native textareas)

// Later
// TODO: insert / replace modes
// TODO: implement 'max_lines' and 'max_chars' to limit textarea content size

/**
 * {1 About this module}
 *
 *
 * {1 Where should I start?}
 *
 */

import stdlib.widgets.core

/**
 * A document view is structured as this:
 * The main <div> containing the document lines, each line being a <div> itself.
 * The active line is split in three parts (materialized as spans):
 * the 'precaret' part stands for the beginning of the line, before the insert
 * cursor. The 'postcaret' represents the part after this point. A line break
 * triggers the creation of a new line / div.
 */

type WTextarea.position = { line: int; col: int }

type WTextarea.config = {
  /** General settings */
  max_lines: option(int)
    /** The maximum number of lines an editor can contain */
  max_chars: option(int)
    /** The maximum number of characters a line can contain */

  /** Display setings */
  lineno: bool
    /** Switch to display or not line numbers on the editor margin */

  /** Styles of the WTextarea elements */
  enabled_editor_style: WStyler.styler
    /** Style of the editor itself when focused */
  disabled_editor_style: WStyler.styler
    /** Style of the editor itself */
  line_style: WStyler.styler
    /** Style of a line */

  /** Callback functions */
  on_text   : string, WTextarea.position, (-> void) -> void
    /** Function called when text is entered in the editor */
  on_enter  : (-> void) -> void
    /** Function called when Enter is pressed */
  on_delete : (-> void) -> void
    /** Function called when pressing Backspace or Delete */
  on_focus  : (-> void) -> void
    /** Function called when the editor is activated */
  on_blur   : (-> void) -> void
    /** Function called when the editor is deactivated */
}

/**
 * At any time, [WTextarea] instances are in a given mode of the following
 * type.
 * TODO: implement these modes: inactive, insert, replace
 * TODO: ensure that modes can handle more complex behaviours (vi mode...)
 */
type WTextarea.mode = {
  enable: WTextarea.config, string -> void
    /** Function called when entering the mode */
  /*leave: void -> void*/
    /** Function called when leaving the mode */
}

/**
 * A simple editing widget in which all editing events can be bound
 * to a handle.
 */
WTextarea =
  get_editor_id(id: string): string = "{id}_editor"

  /*string_change(str: string, delta: int, pos: int, source: string): string =*/
  /*  String.drop_right(String.length(source) - pos + delta, source) ^*/
  /*    str ^ String.drop_left(pos, source)*/

  /* Insert [str] in [source] at position [pos] */
  /*string_insert(str, pos, source) = string_change(str, 0, pos, source)*/

  /* Delete [delta] characters in [source] at position [pos] */
  /*string_delete(delta, pos, source) = string_change("", delta, pos, source)*/

  /* Insert [text] at the beginning of the element referenced by [sel] */
  /*insert_before(text: string, sel: dom): void =*/
  /*  text ^ Dom.get_text(sel)*/
  /*    |> Dom.set_text(sel, _)*/

  get_line_id(editor_id: string, i: int): string = "{editor_id}_line_{i}"
  get_caret_id(editor_id: string): string = "{editor_id}_caret"
  get_precaret_id(editor_id: string): string  = "{editor_id}_precaret"
  get_postcaret_id(editor_id: string): string = "{editor_id}_postcaret"
  get_position_id(editor_id: string): string  = "{editor_id}_position"
  get_buffer_id(editor_id: string): string = "{editor_id}_target"

  get_line_class(editor_id: string): string   = "{editor_id}_line"
  get_line_content_class(editor_id: string): string =
    "{editor_id}_line_content"
  get_current_line_class(editor_id: string): string =
    "{editor_id}_current_line"

{{
  /**
   * {1 High-level interface}
   */

  /**
   * The default [WTextarea] configurations. Callbacks don't have any
   * side-effect here. They only call the given continuation (usually, the
   * continuation updates the editor content).
   */

  /**
   * A base configuration, with no style, used by others default configs
   */
  abstract_config: WTextarea.config = {
      max_lines = none
      max_chars = none

      lineno    = false

      disabled_editor_style = WStyler.empty
      enabled_editor_style = WStyler.empty
      line_style = WStyler.empty

      on_text   = exec_text_cont
      on_enter  = exec_cont
      on_delete = exec_cont
      on_focus  = exec_cont
      on_blur   = exec_cont
    }

  /**
   * A config trying to make [WTextarea] look like a native browser textarea
   */
  native_config: WTextarea.config = {abstract_config with

      disabled_editor_style = WStyler.make_style(css {
          width: 400px; height: 200px;
          border: thin solid gray; overflow: auto; cursor: text;
          /** Font style similar to a native textarea */
          padding: 2px;
          font-size: 0.80em;
          font-family: Monospace;
        })

      enabled_editor_style = WStyler.make_style(css {
          width: 400px; height: 200px;
          border: thin solid goldenrod; overflow: auto; cursor: text;
          /** Font style similar to a native textarea */
          padding: 2px;
          font-size: 0.80em;
          font-family: Monospace;
        })

      line_style = WStyler.make_style(
        {not_typed=("word-wrap", "break-word")} +> css {
          padding: 0em 0 0.30em 0;
          /*height: 1.1em;*/
          width: 100%;
          cursor: text;
          white-space: pre;
        })
    }

  /**
   * A config embedding extra information in the editor (line numbers, line
   * separators...)
   */
  verbose_config: WTextarea.config = {abstract_config with

      lineno    = true

      disabled_editor_style = WStyler.make_style(css {
          width: 400px; height: 200px;
          border: thin solid gray; overflow: auto; cursor: text;
          padding: 2px;
        })

      enabled_editor_style = WStyler.make_style(css {
          width: 400px; height: 200px;
          border: thin solid goldenrod; overflow: auto; cursor: text;
          padding: 2px;
        })

      line_style = WStyler.make_style(
        {not_typed=("word-wrap", "break-word")} +> css {
          border-bottom: 1px dashed gray;
          padding: 0 0 0 0;
          /*height: 1.1em;*/
          width: 100%;
          cursor: text;
        })
    }

  /**
   * The main function to call for building an editor. The returned XHTML
   * can be placed where the editor should appear (usually wrapped in a container
   * which can be created with [WCommon.make]
   */
  edit(config: WTextarea.config, id: string, init_content: string): xhtml =
    /* IDs */
    editor_id  = get_editor_id(id)
    /*editor_sel = #{get_buffer_id(editor_id)}*/
    /*editor_caret_id     = "{editor_id}_caret"*/
    /*editor_precaret_id  = "{editor_id}_precaret"*/
    /*editor_postcaret_id = "{editor_id}_postcaret"*/
    /*editor_position_id  = "{editor_id}_position"*/
    /*editor_line_class   = "{editor_id}_line"*/

    /* The HTML of the widget itself */
      /*<div id={get_tmp_id(id)} style={css {display: none;}}></div>*/
          /*style="color: white; border: none; width: 0px;"*/
          /*onkeypress={keychar_listener(config, editor_id, _)}*/
    <>
      <input id={get_buffer_id(editor_id)} type="text"
          onkeydown={keycode_listener(config, editor_id, _)}
          onready={_evt -> init_editor(config, editor_id, init_content)}
        />
      {<div id={editor_id}>
      </div>
        |> WStyler.add(config.disabled_editor_style, _)}
    </>
  //     apply_patch)


  /**
   * {2 Imperative interface}
   */

  /**
   * Change the operating mode of the editor
   *
   * @param config The configuration of the editor
   * @param id The ID of the editor
   * @param mode The mode in which to set the editor
   */
  set_mode(config: WTextarea.config, id: string, mode: WTextarea.mode)
      : void =
    mode.enable(config, id)

  /**
   * Retrieve the editor content as simple text
   */
  parse(_config: WTextarea.config, id: string): string =
    tmp = text_of_lines(get_editor_id(id))
      |> String.concat("\n", _)
    do Log.debug("parse", "{tmp}")
    tmp

  /**
   * {1 Private functions aimed at internal use}
   *
   * Not to be used outside of the module.
   */

  /**
   * {2 Default config constructors}
   */

  /**
   * Editor style common to the enabled and disabled states.
   */
  @private
  common_default_style = WStyler.make_style(css {
      width: 400px; height: 200px;
      border: thin solid gray; overflow: auto; cursor: text;
      /** Font style similar to a native textarea */
      padding: 2px;
      font-size: 0.80em;
      font-family: Monospace;
    })

  /**
   * {2 Modes}
   *
   * Various default modes are defined below.
   */

  /**
   * The basic input mode, with a blinking caret. Entered characters are
   * immediately inserted at the caret position.
   */
  @private
  insert_mode(flush_timer, caret_timer): WTextarea.mode = {
    enable(config, id): void =
      do WStyler.set_dom(config.enabled_editor_style, id)
      do enable_caret_blinking(id, caret_timer)
      _ = flush_timer.start()
      void
  }

  /**
   * The editor is usually in this mode when not focused. It doesn't react to
   * events and has no caret.
   */
  @private
  inactive_mode(flush_timer, caret_timer): WTextarea.mode = {
    enable(config, id): void =
      _ = flush_timer.stop()
      do WStyler.set_dom(config.disabled_editor_style, id)
      disable_caret_blinking(id, false, caret_timer)
  }

  /*
   * Callback functions of the default config (defined here to be available on
   * client-side as well).
   */
  @private exec_text_cont(_, _, cont: -> void): void = cont()
  @private exec_cont(cont: -> void): void = cont()

  /*
   * Insert [text] at the end of the element referenced by the selector [sel]
   */
  @private
  insert_after(text: string, sel: dom): void =
    Dom.get_text(sel) ^ text
      |> Dom.set_text(sel, _)

  /* Delete a character relatively to the current caret position
   * delete(-1) removes previous character
   * delete(1) removes next character... */
  @private
  remove(pos: int, nchar: int, sel: dom): void =
    txt = Dom.get_text(sel)
    new_text = String.substring(pos, String.length(txt) - nchar, txt)
    Dom.set_text(sel, new_text)

  /* Return a selector containing the [WTextarea] lines */
  @private
  get_lines(editor_id: string): dom =
    /*Dom.select_raw_unsafe(".{get_line_class(editor_id)}")*/
    Dom.select_class(get_line_class(editor_id))
    /*tmp = Dom.select_class(get_line_class(editor_id))*/
    /*  |> Dom.select_inside(Dom.select_children(#{editor_id}), _)*/
    /*do jlog("get_line_class = {get_line_class(editor_id)}")*/
    /*do jlog("get_lines = {Dom.length(tmp)}")*/
    /*tmp*/

  /* Retrieve a selector pointing to the currently edited line */
  @private
  current_line(editor_id: string): dom =
    Dom.select_class(get_current_line_class(editor_id))

  /* Retrieve an ID pointing to the currently edited line */
  @private
  current_line_id(editor_id: string): string =
    current_line(editor_id)
      |> Dom.get_id(_)

  /* Retrieve the currently edited line number */
  @private
  current_line_number(editor_id: string): int =
    (current_line(editor_id)
      |> Dom.index(_)) ? 0

  /* Retrieve the current position of the caret in the current line */
  @private
  current_position(editor_id: string): int =
    /*do Log.debug("DEFAULT", "current_position({editor_id})")*/
    Int.of_string(String.strip(Dom.get_text(#{get_position_id(editor_id)})))

  /* Get the currently edited line as a single string containing both pre and
   * post-caret texts. */
  @private
  get_caret_text(editor_id: string): string =
    Dom.get_text(#{get_precaret_id(editor_id)}) ^
        Dom.get_text(#{get_postcaret_id(editor_id)})

  /**
   * Get the currently edited line text as a pair of two strings: text before
   * the caret and text after the caret.
   */
  @private
  get_caret_texts(editor_id: string): (string, string) =
    (Dom.get_text(#{get_precaret_id(editor_id)}),
        Dom.get_text(#{get_postcaret_id(editor_id)}))

  /* Change pre and post caret texts. */
  @private
  set_caret_texts(editor_id: string, precaret_text: string,
      postcaret_text: string): void =
    do Dom.set_text(#{get_precaret_id(editor_id)}, precaret_text)
    Dom.set_text(#{get_postcaret_id(editor_id)}, postcaret_text)

  /* Test whether or not the given DOM element is the currently edited line
   * (the one with the caret). */
  @private
  is_current_line(editor_id: string, element: dom): bool =
    /*sel_caret =*/
      /*Dom.select_raw_unsafe(".{get_line_class(editor_id)}")*/
      /*Dom.select_class(get_line_class(editor_id))*/
        /*|> Dom.select_inside(Dom.select_children(element), _)*/
    /*  Dom.select_inside(Dom.select_children(element),*/
    /*      #{get_caret_id(editor_id)})*/
    /*Dom.length(sel_caret) == 1 // [element] is the currently edited line*/
    Dom.has_class(element, get_current_line_class(editor_id))

  /* Generic wrapper for [Dom.get_text], behaving the same way whether or not
   * the wanted line is being edited (and thus contains the caret) or not. */
  @private
  get_text(editor_id: string, element: dom): string =
    do Log.debug("get_text", "element = {element}")
    if is_current_line(editor_id, element) then
      /* [element] is the active line */
      get_caret_text(editor_id)
    else
      /* [element] is a common line */
      Dom.select_class(get_line_content_class(editor_id))
        |> Dom.select_inside(element, _)
        |> Dom.get_text(_)

  /* Apply a function on text at a given position, transparently taking the
   * caret out of the way (the function can thus be applied on any line,
   * without knowing if this line is the currently edited one or not). */
  /*apply_at(f: (int, string -> string),*/
  /*    pos: WTextarea.position): void =*/
  /*  line_sel = #{get_line_id(editor_id, pos.line)}*/
  /*  if is_current_line(editor_id, line_sel) then*/
  /*    (precaret_text, postcaret_text) = get_caret_texts(editor_id)*/
  /*    precaret_len = String.length(precaret_text)*/
  /*    if precaret_len < pos.col then // caret is before insert point*/
  /*      new_postcaret_text =*/
  /*        f(pos.col - precaret_len, postcaret_text)*/
  /*      set_caret_texts(editor_id, precaret_text, new_postcaret_text)*/
  /*    else // caret is after insert point*/
  /*      new_precaret_text =*/
  /*        f(pos.col, precaret_text)*/
  /*      set_caret_texts(editor_id, new_precaret_text, postcaret_text)*/
  /*  else*/
  /*    do Dom.get_text(line_sel)*/
  /*      |> f(pos.col, _)*/
  /*      |> Dom.set_text(line_sel, _)*/
  /*    void*/

  /* Insert [text] at a given position [pos] of the WTextarea */
  /*insert(text: string, pos: WTextarea.position): void =*/
  /*  apply_at(string_insert(text, _, _), pos)*/

  /*delete(delta: int, pos: WTextarea.position): void =*/
  /*  apply_at(string_delete(delta, _, _), pos)*/

  /* Append a new node at the caret position */
  @private
  append_node(editor_id: string, node: xhtml): void =
    Dom.transform([#{get_precaret_id(editor_id)} +<- node])

  /* Append text at the caret position */
  @private
  append_text(editor_id: string, txt: string): void =
    /*do Log.debug("DEFAULT", "append_text: insert_after({txt}, {get_precaret_id(editor_id)})")*/
    insert_after(txt, #{get_precaret_id(editor_id)})

  /* Append the character corresponding to a keycode at the caret position */
  @private
  append_keycode(editor_id: string, kc: int): void =
    /*do Log.debug("DEFAULT", "append_keycode")*/
    append_text(editor_id, String.of_utf8_val(kc))

  /**
   * Fill the editor with the given content
   *
   * @param id Identifier of the editor to use
   * @param content Content to fill the editor with
   *
   * TODO: migrate this function to public interface
   */
  @private
  set_text(config: WTextarea.config, editor_id: string, text_content: string)
      : void =
    Dom.transform([#{editor_id} <- lines_of_text(config, editor_id, text_content)])

  /**
   * Create an editor line out of a string
   */
  @private
  xhtml_line_of_text(config: WTextarea.config, editor_id: string,
      text_line: string)
      : xhtml =
    p0 = parser
      | t=((!" " .)+) -> <>{Text.to_string(t)}</>
      | " " -> <>&nbsp;</>
    p = parser l=p0* -> <>{ l }</>
    xhtml_line(config, editor_id, none, Parser.parse(p, text_line))

  /**
   * Create a set of editor lines out of a string, making a new line whenever a
   * new line character is encountered.
   */
  @private
  lines_of_text(config: WTextarea.config, editor_id: string, content: string)
      : xhtml =
    p0 = parser
      | t=((!"\n" .)+) -> xhtml_line_of_text(config, editor_id, Text.to_string(t))
      | "\n" -> <></>
    p = parser l=p0* -> <>{ l }</>
    Parser.parse(p, content)

  /**
   * Retrieve the content of an editor as a list of string (one string per
   * line).
   */
  @private
  text_of_lines(editor_id: string): list(string) =
    get_lines(editor_id)
      |> Dom.fold((elt, acc -> get_text(editor_id, elt) +> acc), [], _)
      |> List.rev(_)

  /* Get the currently edited line length */
  @private
  current_line_length(editor_id: string): int =
    String.length(get_caret_text(editor_id))

  /* Update the caret position information stored in the DOM
   * @param abs_pos_opt Optional new absolute caret position
   * @param delta_pos_opt Optional delta to apply to the caret position.
   */
  @private
  update_caret_position(editor_id: string, abs_pos_opt: option(int),
      delta_pos_opt: option(int))
      : void =
    new_abs_pos = abs_pos_opt ?
        (Dom.get_text(#{get_precaret_id(editor_id)})
          |> String.length(_))
    new_pos = Option.switch((d -> d + new_abs_pos), new_abs_pos, delta_pos_opt)
    String.of_int(new_pos)
      |> Dom.set_text(#{get_position_id(editor_id)}, _)

  /* Update all line IDs by resetting their value to an ascending sequence of
   * integers, reflecting possible insertions / deletions which could have
   * taken place since the last update.
   */
  @private
  update_line_ids(editor_id: string, remember_id: string): string =
    lines = get_lines(editor_id)
    nlines = Dom.length(lines)
    Int.fold((acc, i ->
        line_id = get_line_id(editor_id, i)
        elt = Dom.get(lines, i)
        acc = if Dom.contains_selector(elt, "#{remember_id}")
          then line_id
          else acc
        do Dom.set_property_unsafe(elt, "id", line_id)
        acc),
      remember_id, nlines)

  /* Turn a selection into an option: a [some(selection)] if it actually
   * points to an element, [none] if it's empty. */
  @private
  sel_opt(sel: dom): option(dom) =
    if Dom.length(sel) == 1 then some(sel)
    else none

  /* Get the line following the one designated by the given selector. */
  @private
  next_line(_editor_id: string, line: dom): option(dom) =
    /*Dom.select_raw_unsafe(".{get_line_class(editor_id)}")*/
    /*Dom.select_class(get_line_class(editor_id))*/
    /*  |> Dom.select_inside(_, Dom.select_next_one(line))*/
    /*  |> sel_opt(_)*/
    Dom.select_next_one(line)
      |> sel_opt(_)

  /* Get the line preceding the one designated by the given selector. */
  @private
  prev_line(_editor_id: string, line: dom): option(dom) =
    /*Dom.select_raw_unsafe(".{get_line_class(editor_id)}")*/
    /*Dom.select_class(get_line_class(editor_id))*/
    /*  |> Dom.select_inside(Dom.select_previous_one(line), _)*/
    /*  |> sel_opt(_)*/
    Dom.select_previous_one(line)
      |> sel_opt(_)

  /**
   * Insert the caret to the clicked position: move the caret from [offset]
   * characters on the currently edited line if [line_id_opt] is [none], or
   * on the given line if it's [some(line)].
   */
  @private
  insert_caret(editor_id: string, line_id_opt: option(string), offset: int,
      _evt: Dom.event) : void =
    currently_selected = Dom.get_currently_selected()
    start_at = currently_selected.start_at
    _finish_at = currently_selected.finish_at
    caret_pos = start_at + offset
    Option.lazy_switch((line_id ->
        /* Change the active line */
        caret_pos = max(caret_pos - 1, 0) /* Substract the spacer size */
        line_text      = get_text(editor_id, #{line_id})
        line_length    = String.length(line_text)
        (precaret_text, postcaret_text) =
          if line_text == "" then
            ("", "")
          else
            (String.drop_right(line_length - caret_pos, line_text),
                String.drop_left(caret_pos, line_text))
        change_line(editor_id, #{line_id},
            precaret_text, postcaret_text, caret_pos, true)),
      (->
        /* Don't change the active line, only move the caret */
        precaret_len =
          String.length(Dom.get_text(#{get_precaret_id(editor_id)}))
        /* Workaround subtstracting the wrong value returned by
         * [Dom.get_currently_selected] when the line DIV is empty. */
        caret_pos =
          if precaret_len == 0 then caret_pos - start_at else caret_pos
        move_caret(editor_id, caret_pos - precaret_len)),
      line_id_opt)

  @private
  get_selection(): void =
    currently_selected = Dom.get_currently_selected()
    do Log.debug("INSERT_CARET",
        "start selection: {currently_selected.start_in} @ {currently_selected.start_at}")
    do if currently_selected.is_collapsed then
      Log.debug("INSERT_CARET",
          "is_collapsed: {currently_selected.is_collapsed}")
    else
      Log.debug("INSERT_CARET",
          "finish selection: {currently_selected.finish_in} @ {currently_selected.finish_at}")
    /*if not(currently_selected.is_collapsed) then*/
      // An actual selection has been made
    void

  /* Move the caret relatively to its current position.
   * Don't move if at the end of the buffer. */
  @private
  move_caret(editor_id: string, delta: int): void =
    /*do Log.debug("DEFAULT", "move_caret({editor_id}, {delta})")*/
    current_line = current_line(editor_id)
    (precaret_text, postcaret_text) = get_caret_texts(editor_id)
    whole_text = precaret_text ^ postcaret_text
    new_position = String.length(precaret_text) + delta
    /* Action depending on new caret position */
    if new_position < 0 then
      /* Move the caret to the previous line */
      Option.switch((prev_line ->
          prev_line_text = get_text(editor_id, prev_line)
          prev_line_len = String.length(prev_line_text)
          change_line(editor_id, prev_line, prev_line_text, "",
              prev_line_len, true)),
        void, prev_line(editor_id, current_line))
    else if new_position > String.length(whole_text) then
      /* Move the caret to the next line */
      Option.switch((next_line ->
          next_line_text = get_text(editor_id, next_line)
          change_line(editor_id, next_line, "", next_line_text, 0, true)),
        void, next_line(editor_id, current_line))
    else
      /* Move the caret on the same line */
      new_position = max(min(String.length(precaret_text) + delta,
          String.length(whole_text)), 0)
      new_precaret = String.substring(0, max(new_position, 0), whole_text)
      new_postcaret = String.substring(new_position,
          String.length(whole_text) - new_position, whole_text)
      do set_caret_texts(editor_id, new_precaret, new_postcaret)
      do update_caret_position(editor_id, some(new_position), none)
      focus_editor(editor_id)

  /* Get the XHTML of an edited line (splitted between spans representing the
   * pre-caret text, the caret, and the post-caret text. */
  @private
  edit_line(editor_id: string, _: dom, precaret_text: string,
      postcaret_text: string, pos: int): xhtml =
    precaret_id = get_precaret_id(editor_id)
    /* The 'a' element sole purpose is to grab the focus and receive events
     * intended for the editor. */
    <>
      <span id={get_position_id(editor_id)} style={css {display: none;}}>
        {String.of_int(pos)}
      </span>
      <span id={precaret_id}
          onclick={insert_caret(editor_id, none, 0, _)}
          options:onclick="stop_propagation"
          style={css {
              white-space: pre;
            }}>{precaret_text}</span>
      <span id={get_caret_id(editor_id)}
          style={css {
              visibility: hidden;
              position: relative; left: -1px;
              padding: 0px; margin: 0px 0px 0px 0px;
              border-left: 1px solid black;
              border-right: 1px solid white;
            }}>
      </span>
      <span id={get_postcaret_id(editor_id)}
          onclick={insert_caret(editor_id, none,
              String.length(Dom.get_text(#{precaret_id})), _)}
          options:onclick="stop_propagation"
          style={css {
              white-space: pre;
              position: relative;
              left: -2px;
              width: 100%;
            }}>{postcaret_text}</span>
    </>

  @private
  focus_editor(editor_id: string): void =
    _ = Dom.give_focus(#{get_buffer_id(editor_id)})
    void

  /* Move the cursor to the line with id [line_id] and initialize it
   * with the text pre and post-caret texts. */
  @private
  change_line(editor_id: string, new_line: dom, new_precaret_text: string,
      new_postcaret_text: string, cpos: int, focused: bool): void =
    /*do Log.debug("DEFAULT", "change_line")*/
    /*current_line = Dom.select_parent_one(#{get_precaret_id(editor_id)})*/
    current_line = current_line(editor_id)
    current_line_id = Dom.get_id(current_line)
    do Dom.transform([#{current_line_id} <-
        line_content(editor_id, <>{get_caret_text(editor_id)}</>)])
      /*|> Dom.set_text(current_line, _)*/
    do Dom.remove_content(new_line)
    do Dom.remove_class(current_line, get_current_line_class(editor_id))
    /*new_line_xhtml = edit_line(editor_id, new_line, new_precaret_text,*/
    /*    new_postcaret_text, cpos)*/
    /*Dom.transform([new_line +<- new_line_xhtml])*/
    _ = edit_line(editor_id, new_line,
        new_precaret_text, new_postcaret_text, cpos)
      |> Dom.from_xhtml(_)
      |> Dom.to_selection(_)
      |> Dom.put_at_end(new_line, _)
    do Dom.add_class(new_line, get_current_line_class(editor_id))
    if focused then
      focus_editor(editor_id)

  /* Move the caret at a line ends (beginning or end, depending on
   * [direction]) */
  @private
  move_caret_ends(editor_id: string, id: string, direction: int): void =
    do get_text(editor_id, #{id})
      |> String.length(_)
      |> (_ * direction)
      |> move_caret(editor_id, _)
    void

  /* Move the caret to the end of line */
  @private
  move_caret_end(editor_id: string): void =
    move_caret_ends(editor_id, get_postcaret_id(editor_id), 1)

  /* Move the caret to the beginning of line */
  @private
  move_caret_home(editor_id: string): void =
    move_caret_ends(editor_id, get_precaret_id(editor_id), -1)

  /* Vertically move the caret (up or down to the preceding or following
   * lines).*/
  @private
  vmove_caret(editor_id: string, direction: int): void =
    if direction != 0 then
      current_line = current_line(editor_id)
      nchar = Dom.get_text(#{get_precaret_id(editor_id)})
        |> String.length(_)
        //|> max(Int.of_string(String.strip(Dom.get_text(#{editor_position_id}))), _)
        |> max(current_position(editor_id), _)
      new_line_opt =
        if direction > 0 then next_line(editor_id, current_line)
        else prev_line(editor_id, current_line)
      Option.switch((new_line ->
        new_line_text = get_text(editor_id, new_line)
        line_length = String.length(new_line_text)
        (precaret_text, postcaret_text) =
          (String.drop_right(max(0, line_length - nchar), new_line_text),
          String.drop_left(min(nchar, line_length), new_line_text))
          //line_pos = Int.of_string(String.strip(Dom.get_text(#{editor_position_id})))
        change_line(editor_id, new_line,
            precaret_text, postcaret_text, current_position(editor_id), true)),
        void, new_line_opt)

  @private
  new_line_id(editor_id: string): string = "{editor_id}_line_{Dom.fresh_id()}"

  /**
   * Build the XHTML corresponding to a single line content
   */
  @private
  line_content(editor_id: string, content: xhtml): xhtml =
    <>
      <span class="{get_line_content_class(editor_id)}"
          style={css {
            white-space: pre;
          }}>{content}</span>
      <span class="{editor_id}_ignore"
          style={css {
            white-space: pre;
          }}>&nbsp;</span>
    </>

  @private
  xhtml_line(config: WTextarea.config, editor_id: string,
      line_id_opt: option(string), content: xhtml)
      : xhtml =
    editor_caret_id = get_caret_id(editor_id)
    line_id = Option.lazy_default((-> new_line_id(editor_id)), line_id_opt)
    <div id={line_id}
         class="{get_line_class(editor_id)}"
         options:onclick="stop_propagation"
         onclick={evt ->
             if not(Dom.contains_selector(
                 Dom.select_parent_one(#{editor_caret_id}), "#{line_id}"))
             then
               insert_caret(editor_id, some(line_id), 0, evt)
             else
               insert_caret(editor_id, none,
                   current_line_length(editor_id), evt)
           }>
      {line_content(editor_id, content)}
    </div>
      |> WStyler.add(config.line_style, _)

  /* Append an empty line to the editor with a (returned) unique ID */
  @private
  new_line(config: WTextarea.config, editor_id: string,
      insert_after_caret: bool)
      : string =
    /*do Log.debug("DEFAULT", "new_line")*/
    editor_caret_id = get_caret_id(editor_id)
    new_id = "{editor_id}_line_{Dom.fresh_id()}"
    new_line_xhtml = xhtml_line(config, editor_id, some(new_id), <></>)
    do if insert_after_caret then
      /*do Dom.transform([#{get_tmp_id(editor_id)} <- new_line_xhtml])*/
      /*_ = Dom.put_at_end(Dom.select_parent_one(#{editor_caret_id}), #{new_id})*/
      /*_ = Dom.put_at_end(Dom.select_parent_one(#{editor_caret_id}), #{new_id})*/
      /*_ = Dom.put_at_end(Dom.select_parent_one(#{editor_caret_id}), Dom.of_xhtml(new_line_xhtml))*/
      _ = Dom.put_after(Dom.select_parent_one(#{editor_caret_id}),
          Dom.of_xhtml(new_line_xhtml))
      void
    else
      Dom.transform([#{editor_id} +<- new_line_xhtml])
      /*_ = Dom.from_xhtml(new_line_xhtml)*/
      /*  |> Dom.put_at_end(#{editor_id}, Dom.to_selection(_))*/
      /*void*/
    /*do Log.debug("DEFAULT", "new_line_xhtml = {new_line_xhtml}")*/
    /*update_line_ids(editor_id, new_id)*/
    new_id

  /* Split the current line, move the second part to a new line,
   * and move the cursor at the beginning of this line.
   *
   * "line 1: A|B" becomes:
   * line 1: A
   * line 2: |B
   */
  @private
  carriage_return(config: WTextarea.config, editor_id: string): void =
    editor_postcaret_id = get_postcaret_id(editor_id)
    postcaret_text = Dom.get_text(#{editor_postcaret_id})
    do Dom.remove_content(#{editor_postcaret_id})
    do Dom.set_text(#{get_position_id(editor_id)}, String.of_int(0))
    change_line(editor_id, #{new_line(config, editor_id, true)},
        "", postcaret_text, 0, true)

  /* Join the currently edited line with the preceding or following line
   * (depending on [direction]) */
  @private
  join_lines(editor_id: string, direction: int): void =
    line_to_kill = Dom.select_parent_one(#{get_caret_id(editor_id)})
    line_to_merge_opt =
      if direction < 0 then prev_line(editor_id, line_to_kill)
      else next_line(editor_id, line_to_kill)
    Option.switch((line_to_merge ->
        precaret_text = get_text(editor_id, line_to_merge)
        postcaret_text = get_caret_text(editor_id)
        do if direction < 0 then
          change_line(editor_id, line_to_merge,
              precaret_text, postcaret_text, 0, true)
        else
          change_line(editor_id, line_to_merge,
              postcaret_text, precaret_text, 0, true)
        do Dom.remove(line_to_kill)
        void), void, line_to_merge_opt)

  /* Delete the character preceding or following the caret (depending on
   * [direction]) */
  @private
  delete_char(editor_id: string, direction: int): void =
    text_id = if direction > 0
      then get_postcaret_id(editor_id)
      else get_precaret_id(editor_id)
    text_len = Dom.get_text(#{text_id}) |> String.length(_)
    direction0 = if direction < 0 then 0 else 1
    do if text_len == 0 then
      join_lines(editor_id, direction)
    else
      do remove(direction0, 1, #{text_id})
      update_caret_position(editor_id, none, some(direction0))
    // TODO: modify [update_line_ids] to change "dummy" by [none]
    _ = update_line_ids(editor_id, "dummy")
    void

  // TODO: merge the two following functions. Separation isn't necessary
  // anymore with Dom.event (it was with Dom.event)
  /* Caret manipulation events handling. */
//  @private
//  keycode_listener(config: WTextarea.config, editor_id: string,
//      evt: Dom.event): option(int) =
////     do Log.debug("DEFAULT", "keycode = {Client.get_keycode(evt)}")
////     match Client.get_keycode(evt) with
//    (match evt.key_code with
//    | {none} ->
//      /*do Log.debug("DEFAULT", "keycode_listener: keycode = none")*/
//      none
//    | {~some} ->
//      /*do Log.debug("DEFAULT", "keycode_listener: keycode = {some}")*/
//      (match some with
//      | 8  -> do config.on_delete((-> delete_char(editor_id, -1))) none
//      | 46 -> do config.on_delete((-> delete_char(editor_id, 1))) none
//      | 35 -> do move_caret_end(editor_id) none // End key
//      | 36 -> do move_caret_home(editor_id) none // Home key
//      | 37 -> do move_caret(editor_id, -1) none // Left arrow
//      | 38 -> do vmove_caret(editor_id, -1) none // Up arrow
//      | 39 -> do move_caret(editor_id, 1) none // Right arrow
//      | 40 -> do vmove_caret(editor_id, 1) none // Down arrow
//      | _  -> {~some}))

  /**
   * Append the character added to the buffer input instead of directly using
   * the retrieved keycode.  This enables us to handle properly characters made
   * of dead keys.
   */
  /*@private*/
  /*flush_buffer(editor_id: string, keychar_opt: option(int)): void =*/
    /*do Dom.get_value(#{get_buffer_id(editor_id)})*/
    /*  |> append_text(editor_id, _)*/
    /*do update_caret_position(editor_id, none, none)*/
    /*Dom.set_value(#{get_buffer_id(editor_id)}, "")*/
    /*buffer_content = Dom.get_value(#{get_buffer_id(editor_id)})*/
    /*do Log.debug("DEFAULT", "flush_buffer: {buffer_content}")*/
    /*do match keychar_opt with*/
    /*  | {none} -> if String.length(buffer_content) > 0 then*/
    /*    do append_text(editor_id, buffer_content)*/
    /*    update_caret_position(editor_id, none, none)*/
    /*  | {~some} ->*/
    /*    append_keycode(editor_id, some)*/
    /*Dom.set_value(#{get_buffer_id(editor_id)}, "")*/

  /**
   * Flush the hidden input content to the editor
   */
  @private
  flush_buffer(editor_id: string): void =
    buffer_id = get_buffer_id(editor_id)
    /*do Dom.get_value(#{get_buffer_id(editor_id)})*/
    /*  |> append_text(editor_id, _)*/
    /*do update_caret_position(editor_id, none, none)*/
    /*Dom.set_value(#{get_buffer_id(editor_id)}, "")*/
    buffer_content = Dom.get_value(#{buffer_id})
    if String.length(buffer_content) > 0 then
      do append_text(editor_id, buffer_content)
      do update_caret_position(editor_id, none, none)
      Dom.set_value(#{buffer_id}, "")

  /**
   * Turn on caret blinking. The caret appears and disappears at fixed
   * time spans.
   */
  @private
  enable_caret_blinking(editor_id: string, caret_timer)
      : void =
    caret_sel = #{get_caret_id(editor_id)}
    if not(Dom.has_class(caret_sel, "{editor_id}_blinking_off") ||
        Dom.has_class(caret_sel, "{editor_id}_blinking_on")) then
      do Dom.set_style(caret_sel, css {visibility: visible;})
      do Dom.set_class(caret_sel, "{editor_id}_blinking_on")
      _ = caret_timer.start()
      void

  /**
   * Turn off caret blinking. The caret is then still.
   */
  @private
  disable_caret_blinking(editor_id: string, visible: bool, caret_timer)
      : void =
    caret_sel = #{get_caret_id(editor_id)}
    _ = caret_timer.stop()
    do Dom.remove_class(caret_sel, "{editor_id}_blinking_off")
    do Dom.remove_class(caret_sel, "{editor_id}_blinking_on")
    if visible then
      /*do Dom.set_class(#{editor_caret_id}, "{editor_id}_visible")*/
      Dom.set_style(caret_sel, css {visibility: visible;})
    else
      /*do Dom.set_class(#{editor_caret_id}, "{editor_id}_hidden")*/
      Dom.set_style(caret_sel, css {visibility: hidden;})

  /**
   * Alternate between visible / hidden caret states
   */
  @private
  toggle_caret(editor_id: string): void =
    editor_caret_id = get_caret_id(editor_id)
    if Dom.has_class(#{editor_caret_id}, "{editor_id}_blinking_on") then
      do Dom.set_class(#{editor_caret_id}, "{editor_id}_blinking_off")
      Dom.set_style(#{editor_caret_id}, css {visibility: hidden;})
    else if Dom.has_class(#{editor_caret_id}, "{editor_id}_blinking_off") then
      do Dom.set_class(#{editor_caret_id}, "{editor_id}_blinking_on")
      Dom.set_style(#{editor_caret_id}, css {visibility: visible;})

  /**
   * 'keydown' event handler (intented to catch special key events: arrows and
   * other cursor movement keys, for example.
   */
  @private
  keycode_listener(config: WTextarea.config, editor_id: string,
      event: Dom.event)
      : void =
    match event.key_code with
     | {none} -> void
     | {~some} ->
     do Log.debug("KEYBOARD", "keycode_listener: {some}")
     (match some with
       | 8  -> config.on_delete((-> delete_char(editor_id, -1))) // Backspace
       | 13 -> config.on_enter(-> carriage_return(config, editor_id)) // Enter
       | 35 -> move_caret_end(editor_id) /* End key */
       | 36 -> move_caret_home(editor_id) /* Home key */
       | 37 -> move_caret(editor_id, -1) /* Left arrow */
       | 38 -> vmove_caret(editor_id, -1) /* Up arrow */
       | 39 -> move_caret(editor_id, 1)/* Right arrow */
       | 40 -> vmove_caret(editor_id, 1) /* Down arrow */
       | 46 -> config.on_delete((-> delete_char(editor_id, 1)))
       /*| _ -> flush_buffer(editor_id))*/
       | _ -> void)

  /**
   * 'keypress' event handler (intended to catch entered 'real' characters, as
   * opposed to special keys caught in [keycode_listener])
   */
  @private
  keychar_listener(_config: WTextarea.config, _editor_id: string,
      event: Dom.event)
      : void =
//    do Log.debug("DEFAULT", "which = {WTextarea_client.get_which(evt)}")
//    match WTextarea_client.get_which(evt) with
   /*do Log.debug("DEFAULT", "keychar_listener: keycode = {key_code}")*/
   match event.key_code with
    | {none} -> void
    | {~some} ->
    do Log.debug("KEYBOARD", "keychar_listener: {some}")
    (match some with
      /*| 8  -> config.on_delete((-> delete_char(editor_id, -1))) // Backspace*/
      /*| 13 -> config.on_enter(-> carriage_return(config, editor_id)) // Enter*/
      /*| 32 -> append_node(editor_id, <>&nbsp;</>) // Space*/
      // TODO: do something more interesting without killing performance
      | _ -> void)
        // TODO: give version, line ID and character position to [on_text]
        /*config.on_text(String.of_utf8_val(kc),*/
        /*    {line=current_line_number(editor_id)*/
        /*      col=current_position(editor_id)},*/
        /*    (-> flush_buffer(editor_id))))*/
            /*(-> void )))*/
            /* The answer (ugly hack, that is) to flushing the buffer _after_
             * the new character has been registered in the field. */
            /*(-> sleep(42, (-> flush_buffer(editor_id))))))*/

  @private
  get_code(e):string = match e.key_code with
    | {none} -> "none"
    |~{some} -> Int.to_string(some)

  /**
   * Initialize the editor when first loading the page
   */
  @private
  init_editor(config: WTextarea.config, id: string, init_content: string)
      : void =
    /*do Log.debug("DEFAULT", "init_editor")*/
    editor_sel = #{get_buffer_id(id)}
    /* Create a timer for caret blinking */
    caret_timer = Scheduler.make_timer(500, (->
        toggle_caret(id)))
    /* Create a timer regularly flushing the buffer */
    flush_timer = Scheduler.make_timer(50, (->
        flush_buffer(id)))
    /* Create the first line */
    /*_ = new_line(id, false)*/
    do change_line(id, #{new_line(config, id, false)},
        "", "", 0, false)
    /* Set initial content */
    do set_text(config, id, init_content)
    /* Event bindings */
    _ = Dom.bind(editor_sel, {focus},
        /*enable_editor(config, id, flush_timer, caret_timer))*/
      (_ ->
        set_mode(config, id, insert_mode(flush_timer, caret_timer))))
    _ = Dom.bind(editor_sel, {blur},
        /*disable_editor(config, id, flush_timer, caret_timer))*/
      (_ ->
        set_mode(config, id, inactive_mode(flush_timer, caret_timer))))
    _ = Dom.bind(editor_sel, {keydown},
        (_ -> disable_caret_blinking(id, true, caret_timer)))
    _ = Dom.bind(editor_sel, {keyup},
        (_ -> enable_caret_blinking(id, caret_timer)))
    _ = Dom.bind(#{id}, {click}, (_evt ->
          /*lines = get_lines(id)*/
          /*last_line_idx = Dom.length(lines)*/
          /*last_line = Dom.get(lines, last_line_idx - 1)*/
          /*precaret_text = get_text(id, last_line)*/
          /*change_line(id, last_line, precaret_text, "",*/
          /*  String.length(precaret_text), true)*/
          /*_ = Dom.give_focus(editor_sel)*/
          /*void*/
          focus_editor(id)
        ))
    void


//     apply_patch(patch: WTextarea.patch): void =
//       match patch with
//         | {insert_text=(pos, text)} -> insert(text, pos)
//         | {delete_text=(pos, delta)} -> delete(delta, pos)
//         | _ -> void

}}
