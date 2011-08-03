/**
 * {1 Network infrastructure}
 */

/**
 * The type of messages sent by a client to the chatroom
 */
type message = {author: string /**The name of the author (arbitrary string)*/
               ; text: string  /**Content entered by the user*/}

/**
 * The chatroom.
 */
@publish room = Network.cloud("room"): Network.network(message)

/**
 * {1 User interface}
 */

/**
 * Update the user interface in reaction to reception of a message.
 *
 * This function is meant to be registered with [room] as a callback.
 * Its sole role is to display the new message in [#conversation].
 *
 * @param x The message received from the chatroom
 */
user_update(x: message) =
  line = <div class="line">
            <div class="user">{x.author}:</div>
            <div class="message">{x.text}</div>
         </div>
  do Dom.transform([#conversation +<- line ])
  Dom.scroll_to_bottom(#conversation)

/**
 * Inserts a small graphical component that displays an input and a button.
 *
 * Reads the content of [#entry], clear this content, and call the [action] function on it.
 *
 * @param title The text on the button.
 * @param action The function called when clicking on the button (or pressing enter).
 */
insert_input(title, action) =
  f() = _ = action(Dom.get_value(#entry)); Dom.clear_value(#entry)
  do Dom.transform([#footer <-
    <input id=#entry onnewline={_ -> f()}/>
    <div class="button" onclick={_ -> f()}>{ title }</div> ])
  Dom.give_focus(#entry)

/**
 * Inserts the input that allows to post messages, broadcasting them to other clients.
 */
insert_post_input(author) =
  broadcast(text) = Network.broadcast(~{author text}, room)
  insert_input("Post", broadcast)

/**
 * Inserts the initial input that allows to choose the author's name.
 *
 * When the name is chosen, it inserts the normal post input,
 * specialized to the given author name.
 *
 */
insert_author_input() =
  insert_input("Please pick a name", insert_post_input)

/**
 * Build the user interface for a client.
 *
 * Note: [onready] events are triggered on the browser when the elements are inserted in the page.
 *
 * @return The user interface, ready to be sent by the server to the client on connection.
 */
start() =
  <div id=#header><div id=#logo></div></div>
  <div id=#conversation onready={_ -> Network.add_callback(user_update, room)}></div>
  <div id=#footer onready={_ -> insert_author_input()}>
  </div>

/**
 * {1 Application}
 */

/**
 * Main entry point.
 *
 * Construct an application called "Chat" (users will see the name in the title bar),
 * embedding statically the contents of directory "resources", using the global stylesheet
 * "resources/css.css" and the user interface defined in [start].
 */
server = Server.one_page_bundle("Chat",
       [@static_resource_directory("resources")],
       ["resources/css.css"], start)
