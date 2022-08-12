import { Room, Client } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { State } from "./schema/State";
import { OnJoinCommand } from "./command/OnJoin"
import { OnLeaveCommand } from "./command/OnLeave"
import { OnChangeNameMessageCommand } from "./command/OnChangeMyNameMessage"
import { OnIAmTypingMessageCommand } from "./command/OnIAmTypingMessage"
import { OnPostMessageMessageCommand } from "./command/OnPostMyMessageMessage"

const maxClients = 10
const maxPosts = 100
const patchRate = 1000

export class ChatRoom extends Room<State> {

  dispatcher = new Dispatcher(this)
  
  onCreate (options: any) {
    this.autoDispose = false
    this.maxClients = maxClients
    this.setState(new State({maxUsers: maxClients}))
    this.setPatchRate(patchRate)
    
    this.onMessage('change-my-name', (client, name) => {
      this.dispatcher.dispatch(
        new OnChangeNameMessageCommand(), 
        {maxPosts, name, sessionId: client.sessionId}
      )
    })

    this.onMessage('i-am-typing', (client) => {
      this.dispatcher.dispatch(
        new OnIAmTypingMessageCommand(), 
        {sessionId: client.sessionId}
      )
    })
    
    this.onMessage('post-my-message', (client, text) => {
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
