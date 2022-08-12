import { Message, Post, Posts, User, Users } from '../schema/State'

export type ClientCommandParams = {sessionId : string}

export function addUser(
  {posts, sessionId, users}: 
  ClientCommandParams & {posts: Posts, users: Users}
) {
  const authors: Set<String> = new Set()
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

export function addPost(
  {maxPosts, post, posts}: 
  {maxPosts: number, post: Post; posts: Posts}
) {
  posts.push(post)
  while (posts.length > maxPosts) {
    posts.shift()
  } 
}


