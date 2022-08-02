import { Room, Client } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { ChatRoomState, Message } from "./schema/ChatRoomState";

export class ChatRoom extends Room<ChatRoomState> {

  dispatcher = new Dispatcher(this)
  
  onCreate (options: any) {
    this.maxClients = 10

    this.setState(new ChatRoomState())

    this.onMessage('message', (client, text) => {
      this.state.messages.push(new Message({author: client.sessionId, text}))
    })
  }

  onJoin (client: Client, options: any) {
    console.log(client.sessionId, 'joined!')
  }

  onLeave (client: Client, consented: boolean) {
    console.log(client.sessionId, 'left!')
  }

  onDispose() {
    console.log('room', this.roomId, 'disposing...')
  }

}
