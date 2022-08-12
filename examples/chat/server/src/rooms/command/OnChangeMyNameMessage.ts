import { Command } from '@colyseus/command'
import { ChatRoom } from '../Chat'
import { ClientCommandParams, addPost } from './Common'
import { Notification, Users } from '../schema/State'

type Params = 
  ClientCommandParams & {maxPosts: number; name: string}

export class OnChangeNameMessageCommand extends Command<ChatRoom, Params> {
  execute({ maxPosts, name, sessionId }: Params) {
    const {posts, users} = this.state
    const user = users.get(sessionId)

    if (user) {
      addPost({
        maxPosts, 
        post: new Notification({text: `@${sessionId} has changed name from ${user.name} to ${name}`}), 
        posts: this.state.posts,
      })

      user.name = name
    }
  }
}
