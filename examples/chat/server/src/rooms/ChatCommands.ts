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

type OnMessageMessageCommandParams = {sessionId: string, text : string}

export class OnMessageMessageCommand extends Command<ChatRoom, OnMessageMessageCommandParams> {
  execute({ sessionId, text }: OnMessageMessageCommandParams) {
    this.state.messages.push(new Message({author: sessionId, text}))
  }
}
