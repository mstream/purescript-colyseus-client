import { Room, Client } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { ChatRoomState } from "./schema/ChatRoomState";
import { OnChangeNameMessageCommand, OnJoinCommand, OnLeaveCommand, OnPostMessageMessageCommand } from "./ChatCommands"

const maxClients = 10
const maxPosts = 100
const patchRate = 1000

export class ChatRoom extends Room<ChatRoomState> {

  dispatcher = new Dispatcher(this)
  
  onCreate (options: any) {
    this.autoDispose = false
    this.maxClients = maxClients
    this.setState(new ChatRoomState({maxUsers: maxClients}))
    this.setPatchRate(patchRate)
    
    this.onMessage('changeName', (client, name) => {
      this.dispatcher.dispatch(
        new OnChangeNameMessageCommand(), 
        {maxPosts, name, sessionId: client.sessionId}
      )
    })

    this.onMessage('postMessage', (client, text) => {
      this.dispatcher.dispatch(
        new OnPostMessageMessageCommand(), 
        {maxPosts, sessionId: client.sessionId, text}
      )
    })
  }

  onJoin (client: Client, options: any) {
    this.dispatcher.dispatch(
      new OnJoinCommand(),
      {maxPosts, sessionId: client.sessionId}
    )
  }

  onLeave (client: Client, consented: boolean) {
    this.dispatcher.dispatch(
      new OnLeaveCommand(),
      {maxPosts, sessionId: client.sessionId}
    )
  }

  onDispose() {
    console.log('room', this.roomId, 'disposing...')
  }
}
