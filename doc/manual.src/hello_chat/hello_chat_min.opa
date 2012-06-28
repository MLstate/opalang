/**
 * Minimal version of hello_chat
 * - no style
 * - no @publish
 */

/**
 * {1 Network infrastructure}
 */

/**
 * The type of messages sent by a client to the chatroom
 */
type message = { string author /**The name of the author (arbitrary string)*/
               , string text  /**Content entered by the user*/
               }

/**
 * The chatroom.
 */
room = Network.network(message) (Network.cloud("room"))

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
function user_update(message x) {
    line = <div>{x.author}: {x.text}</div>;
    #conversation =+ line;
    Dom.scroll_to_bottom(#conversation);
}

/**
 * Broadcast text to the [room].
 *
 * Read the contents of [#entry], clear these contents and send the message to [room].
 *
 * @param author The name of the author. Will be included in the message broadcasted.
 */
function broadcast(author) {
    text = Dom.get_value(#entry);
    message = ~{author, text};
    Network.broadcast(message, room);
    Dom.clear_value(#entry);
}

/**
 * Build the user interface for a client.
 *
 * Pick a random author name which will be used throughout the chat.
 *
 * @return The user interface, ready to be sent by the server to the client on connection.
 */
function start() {
  author = Random.string(8);
  <div id=#conversation onready={function(_) { Network.add_callback(user_update, room) }} />
  <input id=#entry onnewline={function(_) { broadcast(author) }} />
  <input type="button" onclick={function(_) { broadcast(author) }} value="Post" />;
}

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
Server.start(Server.http, {title: "Chat", page: start });
