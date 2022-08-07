import { matchMaker } from 'colyseus'
import Arena from '@colyseus/arena'
import { monitor } from '@colyseus/monitor'
import { ChatRoom } from './rooms/Chat'
import express from 'express'
import path from 'path'

export default Arena({
    getId: () => 'Chat',

    initializeGameServer: (gameServer) => {
        const chatRoomName = 'chat'
        gameServer.define(chatRoomName, ChatRoom)
        matchMaker.createRoom(chatRoomName, {})

    },

    initializeExpress: (app) => {
        app.use(
          '/colyseus', 
          monitor()
        )

        app.use(
          '/chat', 
          express.static(path.join(__dirname, 'public/chat'))
        )
    },

    beforeListen: () => {
    }
})
