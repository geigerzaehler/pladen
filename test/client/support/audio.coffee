# Mock the HTMLAudioElement
define ['event-target', 'support/events'], (EventTarget, {Event})->

  instances = []

  class Audio
    constructor: ->
      instances.unshift(this)

    play: ->
      @dispatchEvent Event('playing')

    pause: ->
      @dispatchEvent Event('pause')

    setProgress: (played, duration)->
      @duration = duration
      @dispatchEvent Event 'durationchange'

      @currentTime = played
      @dispatchEvent Event 'timeupdate'

    addEventListener: EventTarget.addEventListener
    dispatchEvent: EventTarget.dispatchEvent
    removeEventListener: EventTarget.removeEventListener


  Audio._mocks = []
  Audio.mock = (ctx)->
    @_mocks.push([ctx, ctx.Audio])
    ctx.Audio = Audio

  Audio.restore = ->
    for [ctx, Audio] in @_mocks
      ctx.Audio = Audio

  Audio.instance = -> instances[0]

  return Audio
