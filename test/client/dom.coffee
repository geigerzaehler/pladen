define ['dom', 'support', 'support/events']
, (
  {dragDropStream},
  {should, nextEvent, $, eventStreamPromise, promiseCall}
  {DragEvent}
)->


  describe 'drag over', ->

    Given 'an element', $('<div>')[0]
    And 'the overStream', -> dragDropStream(@element, 'text').over
    And -> @overStream.subscribe(->)

    When -> dispatchDragEvent @element, 'dragenter', text: 'hey'
    Then 'overStream', nextEvent(should.be.true)

    When -> dispatchDragEvent @element, 'dragenter'
    Then 'overStream', nextEvent(should.be.false)

    When -> dispatchDragEvent @element, 'dragenter', text: 'hey'
    When -> dispatchDragEvent @element, 'dragleave', text: 'hey'
    Then 'overStream', nextEvent(should.be.false)

    When -> dispatchDragEvent @element, 'dragenter', text: 'hey'
    When -> dispatchDragEvent @element, 'dragleave'
    Then 'overStream', nextEvent(should.be.true)

  describe 'drop', ->

    Given 'an element', $('<div>')[0]
    And 'the dropStream', -> dragDropStream(@element, 'text').drop
    And.value 'the drop', -> eventStreamPromise(@dropStream)

    When 'overEvent', -> DragEvent 'dragover', text: 'hey'
    When.value 'preventDefault', -> promiseCall(@overEvent, 'preventDefault')
    When 'dispatch', -> @overEvent.dispatch(@element)
    Then 'preventDefault', should.be.true

    When -> dispatchDragEvent @element, 'dragover', text: 'hey'
    When -> dispatchDragEvent @element, 'drop', text: 'hey'
    Then 'drop', should.equal('hey')


  dispatchDragEvent = (el, name, data)->
    ev = DragEvent name, data
    ev.dispatch(el)
    ev
