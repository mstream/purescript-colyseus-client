import { ArraySchema, MapSchema, Schema, type } from '@colyseus/schema';

export class Message extends Schema {
  @type('string') author = ''
  @type('string') text = ''
  @type('number') timestamp = Date.now()

  constructor({author,text}: {author:string; text:string}) {
    super()
    this.author = author
    this.text = text
  }
}

export class Notification extends Schema {
  @type('string') text = ''
  @type('number') timestamp = Date.now()

  constructor({text}: {text:string}) {
    super()
    this.text = text
  }
}

export class User extends Schema {
  @type('string') name: string = null
  
  constructor({name}: {name: string}) {
    super()
    this.name = name
  }
}

export class ChatRoomState extends Schema {
  @type('number') maxUsers = 0
  @type([Message]) messages = new ArraySchema<Message>()
  @type([Notification]) notifications = new ArraySchema<Notification>()
  @type({map: User}) users = new MapSchema<User>()

  constructor({maxUsers}: {maxUsers: number}) {
    super()
    this.maxUsers = maxUsers
  }
}
