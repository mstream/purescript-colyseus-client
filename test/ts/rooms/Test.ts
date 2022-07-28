import { Room } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { Schema, type } from "@colyseus/schema";

class State extends Schema {
  @type('number') number = 0
  @type('string') string = ""
}

class Options { maxPlayers = 1 }

export class TestRoom extends Room<State> {
    dispatcher = new Dispatcher(this)
    pingIntervalId: NodeJS.Timer | null = null

    onCreate (options: Options) {
        this.maxClients = options.maxPlayers
        this.setState(new State())
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
