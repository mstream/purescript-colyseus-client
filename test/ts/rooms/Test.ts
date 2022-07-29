import { Client, Room } from "colyseus"
import { Dispatcher } from "@colyseus/command"
import { ArraySchema, Schema, type } from "@colyseus/schema"

class State extends Schema {
  @type('number') number = 0
  @type('string') string = ""
  @type([ "string" ]) messages = new ArraySchema<string>()
}

class Options { 
  maxPlayers = 1
  pingEnabled = false
}

export class TestRoom extends Room<State> {
    dispatcher = new Dispatcher(this)
    pingIntervalId: NodeJS.Timer | null = null

    onCreate (options: Options) {
        this.maxClients = options.maxPlayers
        this.setState(new State())

        if (options.pingEnabled) {
          this.pingIntervalId = setInterval(
            () => this.broadcast('ping', 'ping'),
            1000,
          )
        }

        this.onMessage("test", (client: Client, data: any) => {
          console.info(`received '${data}' message  from '${client.sessionId}' client`)
          this.state.messages.push(data)
        });
    }

    onJoin (client: Client, options: any, auth: any) { 
      console.info(`${client.sessionId} has joined`)
    }

    
    onDispose () {
      if (this.pingIntervalId) {
        clearInterval(this.pingIntervalId)
      }
    }
}
