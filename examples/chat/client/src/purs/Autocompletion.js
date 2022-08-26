function getSelectionAnchorNode() {
  const selection = document.getSelection()
  const container = selection.anchorNode

  if (container.nodeType === 3) {
    return container
  }
}

export function getSelectionTextContentImpl() {
  const node = getSelectionAnchorNode()

  if (node) {
    return node.textContent
  }
}

export function setSelectionTextContentImpl(textContent) {
  return function() {
    const node = getSelectionAnchorNode()

    if (node) {
      node.textContent = textContent
      const selection = document.getSelection()
      selection.removeAllRanges()
      const range = document.createRange()
      range.setStart(node, node.length)
      selection.addRange(range)
    }
  }
}


