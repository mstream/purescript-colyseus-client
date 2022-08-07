import { Room, Client } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { ChatRoomState } from "./schema/ChatRoomState";
import { OnJoinCommand, OnLeaveCommand, OnMessageMessageCommand } from "./ChatCommands";

const maxClients = 10
const patchRate = 1000

export class ChatRoom extends Room<ChatRoomState> {

  dispatcher = new Dispatcher(this)
  
  onCreate (options: any) {
    this.autoDispose = false
    this.maxClients = maxClients
    this.setState(new ChatRoomState({maxUsers: maxClients}))
    this.setPatchRate(patchRate)

    this.onMessage('message', (client, text) => {
      this.dispatcher.dispatch(
        new OnMessageMessageCommand(), 
        {sessionId: client.sessionId, text}
      )
    })
  }

  onJoin (client: Client, options: any) {
    this.dispatcher.dispatch(
      new OnJoinCommand(),
      {sessionId: client.sessionId}
    )
  }

  onLeave (client: Client, consented: boolean) {
    this.dispatcher.dispatch(
      new OnLeaveCommand(),
      {sessionId: client.sessionId}
    )
  }

  onDispose() {
    console.log('room', this.roomId, 'disposing...')
  }
}
