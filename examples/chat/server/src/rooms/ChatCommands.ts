import { ArraySchema, MapSchema } from '@colyseus/schema';
import { Command } from '@colyseus/command'
import { ChatRoom } from './Chat'
import { Message, Notification, Post, User } from './schema/ChatRoomState'

type PostProducingCommand = {maxPosts : number}
type UserAddingCommand = {posts : ArraySchema<Post>}

type OnChangeNameMessageCommandParams = 
  PostProducingCommand & {name: string; sessionId: string}

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
  PostProducingCommand & UserAddingCommand & {sessionId: string}

export class OnJoinCommand extends Command<ChatRoom, OnJoinCommandParams> {
  execute({ maxPosts, posts, sessionId }: OnJoinCommandParams) {
    addPost({
        maxPosts, 
        post: new Notification({text: `${sessionId} has joined`}),
        posts: this.state.posts,
    })
    addUser({
      maxPosts, 
      posts: this.state.posts,
      sessionId, 
      users: this.state.users,
    })
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
      user.leftAt = Date.now()
    }
  }
}

type OnPostMessageMessageCommandParams = 
  PostProducingCommand & {sessionId: string; text : string}

export class OnPostMessageMessageCommand extends Command<ChatRoom, OnPostMessageMessageCommandParams> {
  execute({ maxPosts, sessionId, text }: OnPostMessageMessageCommandParams) {
    addPost({
        maxPosts, 
        post: new Message({author: sessionId, text}),
        posts: this.state.posts,
    })
  }
}

function addUser({posts, sessionId, users}: PostProducingCommand & UserAddingCommand & {sessionId: string; users: MapSchema<User>}) {
  const authors = new Set()
  posts.forEach(post => {
    if (post instanceof Message) {
      authors.add(post.author)
    }
  })
  users.forEach((user, sessionId) => {
    if (user.leftAt !== null && !authors.has(sessionId)) {
      users.delete(sessionId) 
    } 
  })
  users.set(sessionId, new User({name: sessionId}))
}

function addPost({maxPosts, post, posts}: PostProducingCommand & {post: Post; posts: ArraySchema<Post>}) {
  posts.push(post)
  while (posts.length > maxPosts) {
    posts.shift()
  } 
}
