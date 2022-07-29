import { Room, Client } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { ChatRoomState } from "./schema/ChatRoomState";

export class ChatRoom extends Room<ChatRoomState> {

  dispatcher = new Dispatcher(this)
  pingIntervalId: NodeJS.Timer | null = null
  
  onCreate (options: any) {
    this.maxClients = 10
    this.pingIntervalId = setInterval(
      () => this.broadcast('ping', 'ping'),
      1000,
    )

    this.setState(new ChatRoomState())

    this.onMessage("type", (client, message) => {
      //
      // handle "type" message
      //
    })

  }

  onJoin (client: Client, options: any) {
    console.log(client.sessionId, "joined!")
  }

  onLeave (client: Client, consented: boolean) {
    console.log(client.sessionId, "left!")
  }

  onDispose() {
    console.log("room", this.roomId, "disposing...")
    clearInterval(this.pingIntervalId)
  }

}
