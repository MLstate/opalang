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
 * Broadcast text to the [room].
 *
 * Read the contents of [#entry], clear these contents and send the message to [room].
 *
 * @param author The name of the author. Will be included in the message broadcasted.
 */
broadcast(author) =
   do Network.broadcast({~author text=Dom.get_value(#entry)}, room)
   Dom.clear_value(#entry)

start(name) =
   Dom.transform([#body <-
   <div id=#conversation onready={_ -> do Network.add_callback(user_update, room); Dom.give_focus(#entry)}></div>
   <div id=#footer>
      <input id=#entry  onnewline={_ -> broadcast(name)}/>
      <div class="button" onclick={_ -> broadcast(name)}>Post</div>
   </div>])

/**
 * Ask the client to pick a name.
 *
 * @return The user interface, ready to be sent by the server to the client on connection.
 */
init() =
   <>
     <div id=#header><div id=#logo></div></div>
     <div id=#body>
       Please pick a name:
       <input id=#name onnewline={_ -> start(Dom.get_value(#name))} />
       <button onclick={_ -> start(Dom.get_value(#name))}> Join </button>
     </div>
   </>

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
       ["resources/css.css"], init)
