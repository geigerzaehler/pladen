define ['emitter_observer'], (EmitterObserver)->

  # Trigger and observe events on a Bus.
  class BusClient extends EmitterObserver

    connect: EmitterObserver::observe

    disconnect: EmitterObserver::stop

    trigger: ->
      @target?.trigger.apply(@target, arguments)
