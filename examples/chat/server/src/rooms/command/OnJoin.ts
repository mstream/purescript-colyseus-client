import { Command } from '@colyseus/command'
import { ChatRoom } from '../Chat'
import { ClientCommandParams, addPost, addUser } from './Common'
import { Notification } from '../schema/State'

type Params = 
  ClientCommandParams & {maxPosts: number} 

export class OnJoinCommand extends Command<ChatRoom, Params> {
  execute({ maxPosts, sessionId }: Params) {
    const {posts, users} = this.state

    addPost({
        maxPosts, 
        post: new Notification({text: `@${sessionId} has joined`}),
        posts,
    })

    addUser({
      posts,
      sessionId, 
      users,
    })
  }
}

