import { Command } from '@colyseus/command'
import { ChatRoom } from './Chat'
import { Message, Notification, User } from './schema/ChatRoomState'

type OnChangeNameMessageCommandParams = {name: string, sessionId: string}

export class OnChangeNameMessageCommand extends Command<ChatRoom, OnChangeNameMessageCommandParams> {
  execute({ name, sessionId }: OnChangeNameMessageCommandParams) {
    const user = this.state.users.get(sessionId)
    if (user) {
      this.state.notifications.push(new Notification({text: `${user.name} has changed name to ${name}`}))
      user.name = name
    }
  }
}

type OnJoinCommandParams = {sessionId: string}

export class OnJoinCommand extends Command<ChatRoom, OnJoinCommandParams> {
  execute({ sessionId }: OnJoinCommandParams) {
    const name = sessionId
    this.state.notifications.push(new Notification({text: `${name} has joined`}))
    this.state.users.set(sessionId, new User({name}))
  }
}

type OnLeaveCommandParams = {sessionId: string}

export class OnLeaveCommand extends Command<ChatRoom, OnLeaveCommandParams> {
  execute({ sessionId }: OnLeaveCommandParams) {
    const user = this.state.users.get(sessionId)
    if (user) {
      this.state.notifications.push(new Notification({text: `${user.name} has left`}))
      this.state.users.delete(sessionId)
    }
  }
}

type OnPostMessageMessageCommandParams = {sessionId: string, text : string}

export class OnPostMessageMessageCommand extends Command<ChatRoom, OnPostMessageMessageCommandParams> {
  execute({ sessionId, text }: OnPostMessageMessageCommandParams) {
    this.state.messages.push(new Message({author: sessionId, text}))
  }
}
