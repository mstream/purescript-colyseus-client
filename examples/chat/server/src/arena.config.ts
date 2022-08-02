import Arena from "@colyseus/arena";
import { monitor } from "@colyseus/monitor";
import cors from 'cors'
import { ChatRoom } from "./rooms/Chat";

export default Arena({
    getId: () => "Chat",

    initializeGameServer: (gameServer) => {
        gameServer.define('chat', ChatRoom);

    },

    initializeExpress: (app) => {
        app.use(cors());
        app.use("/colyseus", monitor());
    },


    beforeListen: () => {
    }
});
