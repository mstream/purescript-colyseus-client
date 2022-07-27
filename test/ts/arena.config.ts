import Arena from "@colyseus/arena";
import { Room } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { Schema, type } from "@colyseus/schema";
import { monitor } from "@colyseus/monitor";
import cors from 'cors';

class TestState extends Schema {
  @type('number') number = 0
  @type('string') string = ""
}

class TestOptions { maxPlayers = 1 }

class TestRoom extends Room<TestState> {
    dispatcher = new Dispatcher(this)

    onCreate (options: TestOptions) {
        this.maxClients = options.maxPlayers
        this.setState(new TestState())
    }
}

const scenarioIds = ['test1', 'test2', 'test3']

export default Arena({
    getId: () => "Test App",

    initializeGameServer: (gameServer) => {
        scenarioIds.forEach(scenarioId =>
          gameServer.define(scenarioId, TestRoom).enableRealtimeListing()
        )
    },

    initializeExpress: (app) => {
        app.use(cors)
        app.use('/colyseus', monitor());
    },
})

