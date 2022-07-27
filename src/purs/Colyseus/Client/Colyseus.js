import * as Colyseus from "colyseus.js";

export function makeClientImpl({ endpoint }) {
  return new Colyseus.Client(endpoint)
}

export function getAvailableRoomsImpl(client, { roomName }) {
  return async function() {
    return client.getAvailableRooms(roomName)
  }
}

export function joinOrCreateImpl(client, { options, roomName }) {
  return async function() {
    return client.joinOrCreate(roomName, options)
  }
}

