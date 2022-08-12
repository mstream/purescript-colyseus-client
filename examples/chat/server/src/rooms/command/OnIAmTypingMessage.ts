import { Command } from '@colyseus/command'
import { ChatRoom } from '../Chat'
import { ClientCommandParams } from './Common'

type Params = 
  ClientCommandParams 

export class OnIAmTypingMessageCommand extends Command<ChatRoom, Params> {
  execute({ sessionId }: Params) {
    this.room.broadcast('is-typing', {sessionId, timestamp: Date.now()})
  }
}


