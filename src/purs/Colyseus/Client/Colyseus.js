import * as Colyseus from "colyseus.js";

export function makeClientImpl({ endpoint }) {
  return new Colyseus.Client(endpoint)
}

export function joinOrCreateImpl(client, { roomName }) {
  return async function() {
    return client.joinOrCreate(roomName)
  }
}

export function getStateImpl(room) {
  return function() {
    return new Promise(resolve => {
      room.onStateChange.once((state) => {
        resolve(state)
      })
    })
  }
}

export function getSessionIdImpl(room) {
  return room.sessionId
}

