import { ArraySchema } from '@colyseus/schema';
import { Command } from '@colyseus/command'
import { ChatRoom } from './Chat'
import { Message, Notification, Post, User } from './schema/ChatRoomState'

type PostProducingCommand = {maxPosts : number}

type OnChangeNameMessageCommandParams = 
  PostProducingCommand & {name: string, sessionId: string}

export class OnChangeNameMessageCommand extends Command<ChatRoom, OnChangeNameMessageCommandParams> {
  execute({ maxPosts, name, sessionId }: OnChangeNameMessageCommandParams) {
    const user = this.state.users.get(sessionId)
    if (user) {
      addPost({
        maxPosts, 
        post: new Notification({text: `${user.name} has changed name to ${name}`}), 
        posts: this.state.posts,
      })
      user.name = name
    }
  }
}

type OnJoinCommandParams = 
  PostProducingCommand & {sessionId: string}

export class OnJoinCommand extends Command<ChatRoom, OnJoinCommandParams> {
  execute({ maxPosts, sessionId }: OnJoinCommandParams) {
    const name = sessionId
    addPost({
        maxPosts, 
        post: new Notification({text: `${name} has joined`}),
        posts: this.state.posts,
    })
    this.state.users.set(sessionId, new User({name}))
  }
}

type OnLeaveCommandParams = 
  PostProducingCommand & {sessionId: string}

export class OnLeaveCommand extends Command<ChatRoom, OnLeaveCommandParams> {
  execute({ maxPosts, sessionId }: OnLeaveCommandParams) {
    const user = this.state.users.get(sessionId)
    if (user) {
      addPost({
          maxPosts, 
          post: new Notification({text: `${user.name} has left`}),
          posts: this.state.posts,
      })
      this.state.users.delete(sessionId)
    }
  }
}

type OnPostMessageMessageCommandParams = 
  PostProducingCommand & {sessionId: string, text : string}

export class OnPostMessageMessageCommand extends Command<ChatRoom, OnPostMessageMessageCommandParams> {
  execute({ maxPosts, sessionId, text }: OnPostMessageMessageCommandParams) {
    addPost({
        maxPosts, 
        post: new Message({author: sessionId, text}),
        posts: this.state.posts,
    })
  }
}

function addPost({maxPosts, post, posts}: PostProducingCommand & {post: Post; posts: ArraySchema<Post>}) {
  posts.push(post)
  while (posts.length > maxPosts) {
    posts.shift()
  } 
}
