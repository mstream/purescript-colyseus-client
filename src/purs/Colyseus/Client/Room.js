export function getSessionIdImpl(room) {
  return room.sessionId
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
