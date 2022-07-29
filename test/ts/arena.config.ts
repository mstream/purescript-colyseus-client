import Arena from '@colyseus/arena'
import { monitor } from '@colyseus/monitor'
import cors from 'cors'
import {TestRoom} from './rooms/Test'

const scenarioIds = ['test1', 'test2', 'test3', 'test4', 'test5']

export default Arena({
    getId: () => "Colyseus Server",

    initializeGameServer: (gameServer) => {
        scenarioIds.forEach(scenarioId =>
          gameServer.define(scenarioId, TestRoom).enableRealtimeListing()
        )
    },

    initializeExpress: (app) => {
        app.use(cors())
        app.use('/colyseus', monitor())
    },
})

