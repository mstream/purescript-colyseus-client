import { Command } from '@colyseus/command'
import { ChatRoom } from '../Chat'
import { ClientCommandParams, addPost, addUser } from './Common'
import { Message } from '../schema/State'

type Params = 
  ClientCommandParams & {maxPosts: number; text : string}

export class OnPostMessageMessageCommand extends Command<ChatRoom, Params> {
  execute({ maxPosts, sessionId, text }: Params) {
    const {posts} = this.state

    addPost({
        maxPosts, 
        post: new Message({author: sessionId, text}),
        posts,
    })
  }
}
