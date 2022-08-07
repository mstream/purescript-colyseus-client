export function scrollIntoViewImpl(element) {
  return function() {
    element.scrollIntoView(false)
  }
}
