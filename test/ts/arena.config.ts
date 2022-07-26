import Arena from "@colyseus/arena";
import { Room } from "colyseus";
import { Dispatcher } from "@colyseus/command";
import { Schema, type } from "@colyseus/schema";
import { monitor } from "@colyseus/monitor";
import cors from 'cors';

class TestState extends Schema {
  @type('number') number: number = 0;
  @type('string') string: string = "";
}

class TestRoom extends Room<TestState> {
    dispatcher = new Dispatcher(this)
    maxClients = 1

    onCreate () {
        this.setState(new TestState())
    }
}

export default Arena({
    getId: () => "Test App",

    initializeGameServer: (gameServer) => {
        gameServer.define("test", TestRoom).enableRealtimeListing();
    },

    initializeExpress: (app) => {
        app.use(cors)
        app.use('/colyseus', monitor());
    },
})

