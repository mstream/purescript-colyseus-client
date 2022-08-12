import { ArraySchema, MapSchema, Schema, type } from '@colyseus/schema';

export class Post extends Schema {
  @type('string') tag = ''
  @type('string') text = ''
  @type('number') timestamp = Date.now()

  constructor({tag, text}: {tag: string, text:string}) {
    super()
    this.tag = tag
    this.text = text
  }
}

export type Posts = ArraySchema<Post>
export type Users = MapSchema<User>

export class Notification extends Post {
  constructor({text}: {text:string}) {
    super({tag: 'notification', text})
  }
}

export class Message extends Post {
  @type('string') author = ''

  constructor({author,text}: {author:string; text:string}) {
    super({tag: 'message', text})
    this.author = author
  }
}

export class User extends Schema {
  @type('string') name: string = null
  @type('number') leftAt: number = null
  
  constructor({name}: {name: string}) {
    super()
    this.name = name
  }
}

export class State extends Schema {
  @type('number') maxUsers = 0
  @type([Post]) posts = new ArraySchema<Post>()
  @type({map: User}) users = new MapSchema<User>()

  constructor({maxUsers}: {maxUsers: number}) {
    super()
    this.maxUsers = maxUsers
  }
}
