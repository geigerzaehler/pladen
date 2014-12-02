define ['chai', 'when', 'jquery', 'chai-builder', 'chai-jquery', 'bacon', 'sinon']
, (chai, Promise, $, chaiBuilder, chaiJquery, Bacon)->
  chai.use(chaiJquery)
  chai.use(chaiBuilder)
  should = chai.should
  expect = chai.expect

  nextEvent = (exp)->
    new NextEventExpectation(exp)

  class NextEventExpectation

    constructor: (@valueExpectation)->
      @label = 'next event ' + @valueExpectation.label

    test: (observable)->
      Promise.promise (resolve, reject)->
        observable.toEventStream().subscribe (event)->
          if event.isInitial()
            return

          if event.isNext() and event.hasValue()
            resolve(event.value())
          else
            reject(event)
          Bacon.noMore
      .then (val)=>
        @valueExpectation.test(val)


  # Create a promise from a Bacon Observable.
  #
  # The promise is fullfilled when the next non-initial event is
  # emitted.
  eventStreamPromise = (observable)->
    Promise.promise (resolve, reject)->
      observable.subscribe (event)->
        if event.isInitial()
          return

        if event.isNext() and event.hasValue()
          resolve(event.value())
        else
          reject(event)
        Bacon.noMore

  promiseProperty = (property)->
    Promise.promise (resolve, reject)->
      property.subscribe (event)->
        if event.isInitial()
          resolve(event.value())
        else
          reject(event)
        return Bacon.noMore


  # Return a promise that is fullfilled when the method is called on
  # the object.
  promiseCall = (object, property)->
    deferred = Promise.defer()

    method = object[property]
    object[property] = wrapper = (args...)->
      deferred.resolve(true)
      method.apply(this, args)
    wrapper.restore = ->
      object[property] = method

    return deferred.promise


  return {
    expect, should, nextEvent, Promise, $, w: Promise,
    eventStreamPromise, promiseProperty, promiseCall, sinon
  }
