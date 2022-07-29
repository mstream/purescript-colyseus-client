import { Schema, Context, type } from "@colyseus/schema";

export class ChatRoomState extends Schema {

  @type("string") mySynchronizedProperty: string = "Hello world";

}
