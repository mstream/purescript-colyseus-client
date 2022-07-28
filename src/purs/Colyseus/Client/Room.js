export function addMessageListenerImpl(room, messageName, listener) {
  return async function() {
    room.onMessage(messageName, msg => listener(msg)())
  }
}

export function getIdImpl(room) {
  return room.id
}

export function getSessionIdImpl(room) {
  return room.sessionId
}

export function getStateImpl(room) {
  return room.state
}

export function leaveImpl(room) {
  return function() {
    return room.leave()
  }
}

export function requestStateImpl(room) {
  return function() {
    return new Promise(resolve => {
      room.onStateChange.once((state) => {
        resolve(state)
      })
    })
  }
}
