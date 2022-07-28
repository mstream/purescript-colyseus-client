import { Room } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { Schema } from "@colyseus/schema";


export class ChatRoom extends Room {
    dispatcher = new Dispatcher(this)
    pingIntervalId: NodeJS.Timer | null = null

    onCreate () {
        this.maxClients = 10
        this.pingIntervalId = setInterval(
          () => this.broadcast('ping', 'ping'),
          1000,
        )
    }
    
    onDispose () {
      if (this.pingIntervalId) {
        clearInterval(this.pingIntervalId)
      }
    }
}
