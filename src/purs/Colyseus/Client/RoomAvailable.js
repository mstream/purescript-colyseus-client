export function getClientsImpl(roomAvailable) {
  return roomAvailable.clients
}

export function getMaxClientsImpl(roomAvailable) {
  return roomAvailable.maxClients
}

export function getRoomIdImpl(roomAvailable) {
  return roomAvailable.roomId
}
