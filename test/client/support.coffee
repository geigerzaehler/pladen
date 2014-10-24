define ['chai', 'when', 'jquery', 'chai-builder', 'chai-jquery', 'bacon']
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


  return {expect, should, nextEvent, Promise, $, eventStreamPromise}
