define ->

  class DataTransfer
    constructor: (@data = {})->
      @types = Object.getOwnPropertyNames(@data)
      @types.contains = (x)-> this.indexOf(x) >= 0

    getData: (key)->
      d = @data[key]
      if typeof d != 'string'
        JSON.stringify(d)
      else
        d

  Event = (type)->
    ev = document.createEvent 'HTMLEvents'
    ev.initEvent(type)
    ev.dispatch = (el)-> el.dispatchEvent(this)
    ev


  DragEvent = (type, data = {})->
    ev = Event type
    ev.dataTransfer = new DataTransfer data
    ev

  return {Event, DragEvent}
