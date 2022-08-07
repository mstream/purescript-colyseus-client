import { Command } from "@colyseus/command";
import { ChatRoom } from "./Chat";
import { Message, Notification, User } from "./schema/ChatRoomState";

type OnJoinCommandParams = {sessionId: string}

export class OnJoinCommand extends Command<ChatRoom, OnJoinCommandParams> {
  execute({ sessionId }: OnJoinCommandParams) {
    this.state.users.set(sessionId, new User({name: sessionId}))
    this.state.notifications.push(new Notification({text: `${sessionId} has joined`}))
  }
}

type OnLeaveCommandParams = {sessionId: string}

export class OnLeaveCommand extends Command<ChatRoom, OnLeaveCommandParams> {
  execute({ sessionId }: OnLeaveCommandParams) {
    this.state.users.delete(sessionId)
    this.state.notifications.push(new Notification({text: `${sessionId} has left`}))
  }
}


type OnMessageMessageCommandParams = {sessionId: string, text : string}

export class OnMessageMessageCommand extends Command<ChatRoom, OnMessageMessageCommandParams> {
  execute({ sessionId, text }: OnMessageMessageCommandParams) {
    this.state.messages.push(new Message({author: sessionId, text}))
  }
}
