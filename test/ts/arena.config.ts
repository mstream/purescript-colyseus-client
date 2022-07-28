import Arena from '@colyseus/arena'
import { monitor } from '@colyseus/monitor'
import cors from 'cors'
import {ChatRoom} from './rooms/Chat'
import {TestRoom} from './rooms/Test'

const scenarioIds = ['test1', 'test2', 'test3', 'test4']

export default Arena({
    getId: () => "Colyseus Server",

    initializeGameServer: (gameServer) => {
        scenarioIds.forEach(scenarioId =>
          gameServer.define(scenarioId, TestRoom).enableRealtimeListing()
        )
        gameServer.define("chat", ChatRoom).enableRealtimeListing()
    },

    initializeExpress: (app) => {
        app.use(cors())
        app.use('/colyseus', monitor());
    },
})

