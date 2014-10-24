define ['dom', 'support']
, ({dragDropStream}, {should, nextEvent, $, Promise, eventStreamPromise})->


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

    When -> dispatchDragEvent @element, 'drop', text: 'hey'
    Then 'drop', should.equal('hey')


  class DataTransfer
    constructor: (@data = {})->
      @types = Object.getOwnPropertyNames(@data)
      @types.contains = (x)-> this.indexOf(x) >= 0

    getData: (key)-> @data[key]

  Event = (type)->
    e = document.createEvent 'HTMLEvents'
    e.initEvent(type)
    e

  dispatchDragEvent = (el, name, data)->
    ev = Event name
    ev.dataTransfer = new DataTransfer data
    el.dispatchEvent(ev)
