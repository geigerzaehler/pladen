define ->

  # Observe events on a single emitter and run methods.
  #
  # The target of an observer is the object to watch for
  # events, the context is the object to call methods on when
  # an event is fired.
  #
  #   o = new Observer(ctx, target)
  #   o.on('update', 'render')
  #   target.emit('update', 1)
  #   # calls ctx.render(1)
  #   o.observe(anotherTarget)
  #
  # The target must provide 'on' and 'off' methods.
  class EmitterObserver

    constructor: (@ctx, @target)->
      @_observed = []

    # Call '@ctx[method]' when 'event' is fired on '@target'.
    #
    # If there is no target yet, it records the event and method
    # to be later added to a target with 'start()'.
    on: (event, method)->
      callback = @ctx[method].bind(@ctx)
      @_observed.push([event, callback])

      if @target
        @target.on event, callback

    # Register event handlers on 'target'.
    #
    # Without the target parameter it re-registers all event
    # handlers on the current target. Also Remove all listeners
    # to previous targets.
    observe: (target = @target)->
      @stop()
      @target = target
      for [event, callback] in @_observed
        @target.off event, callback
      
    # Unregister all event handlers from the target
    stop: ->
      for [event, callback] in @_observed
        @target.off event, callback
      @target = null
