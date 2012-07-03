/**
 * The type of messages sent by a client to the chatroom
 */
type message = { string author /**The name of the author (arbitrary string)*/
               , string text  /**Content entered by the user*/
               }

module Model {

  /**
   * The chatroom.
   */
  private Network.network(message) room = Network.cloud("room")

  exposed function broadcast(message) {
    Network.broadcast(message, room);
  }

  function register_message_callback(callback) {
    Network.add_callback(callback, room);
  }

  function new_author() {
    Random.string(8);
  }

}
