define ->

  # Register event handlers that call methods on an object.
  #
  # An observer is constructed with a context object. We then
  # register event handlers on emitters that call methods on the
  # context. Emitters must provide 'on' and 'off' methods.
  class Observer

    constructor: (@ctx)->
      @_observed ||= []

    # Call '@ctc[method]' when 'emitter' fires the event
    observe: (emitter, event, method)->
      method = @ctx[method]
      callback = method.bind(@ctx)

      @_observed.push([emitter, event, callback])

      emitter.on event, callback

    # Unregister all event handlers.
    stop: ->
      for [emitter, event, callback] in @_observed
        emitter.off event, callback

    # Re-register all event handlers previously added with
    # 'observe()'.
    start: ->
      @stop()
      for [emitter, event, callback] in @_observed
        emitter.on event, callback
