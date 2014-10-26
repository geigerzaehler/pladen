# Mock the HTMLAudioElement
define ['event-target', 'support/events'], (EventTarget, {Event})->

  class Audio
    play: ->
      @dispatchEvent Event('playing')

    pause: ->
      @dispatchEvent Event('pause')


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

  return Audio
