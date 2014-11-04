define ['track-context-menu', 'services', 'support']
,      (menuService, {Provider, service}, {should, sinon})->

  describe 'track context menu', ->

    Given 'track', -> {}
    Given 'services', -> new Provider
    Given 'player', -> {play: sinon.stub()}
    Given 'el', -> $('<div><button data-track-id=2>')
    Given 'register menu', ->
      @services.provide('track-context-menu', menuService)
      @services.provide('player', service => @player)
      @services.get('track-context-menu')


    When -> @registerMenu(@el, => @track)
    When -> @el.find('button').click()
    When -> $('.track-context-menu [data-action=play]').click()
    Then 'player', ({play})-> sinon.assert.calledOnce(play)
    Then 'player', ({play})-> sinon.assert.calledWith(play, @track)
