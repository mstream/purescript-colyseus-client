export function eraseContentImpl(element) {
  return function() {
    element.innerHTML = ''
  }
}

export function getInnerTextImpl(element) {
  return function() {
    return element.innerText
  }
}

export function scrollIntoViewImpl(element) {
  return function() {
    element.scrollIntoView(false)
  }
}
