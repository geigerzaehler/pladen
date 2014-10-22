define ['chai', 'when', 'jquery', 'chai-builder']
, (chai, Promise, $, chaiBuilder)->
  chai.use(chaiBuilder)
  should = chai.should
  expect = chai.expect


  # Create a promise from a Bacon Observable.
  #
  # The promise is fullfilled when the next non-initial event is
  # emitted.
  eventStreamPromise = (observable)->
    unsub = null
    Promise.promise (resolve, reject)->
      unsub = observable.subscribe (event)->
        if event.hasValue()
          if not event.isInitial()
            resolve(event.value())
        else
          reject(event)
    .finally -> unsub()


  return {expect, should, Promise, $, eventStreamPromise}
