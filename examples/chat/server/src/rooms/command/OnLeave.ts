import { Command } from '@colyseus/command'
import { ChatRoom } from '../Chat'
import { ClientCommandParams, addPost } from './Common'
import { Notification } from '../schema/State'

type Params = 
  ClientCommandParams & {maxPosts: number} 
   

export class OnLeaveCommand extends Command<ChatRoom, Params> {
  execute({ maxPosts, sessionId }: Params) {
    const {posts, users} = this.state
    const user = users.get(sessionId)

    if (user) {
      addPost({
          maxPosts, 
          post: new Notification({text: `@${user.name} has left`}),
          posts,
      })

      user.leftAt = Date.now()
    }
  }
}

