define ['track-context-menu', 'services', 'support']
,      (menuService, {Provider, service}, {should, sinon, expect, w})->

  describe 'track context menu', ->

    Given 'track', -> {}
    Given 'player', -> {play: sinon.stub()}
    Given 'track el', -> $('<div><button data-track-id=2>')
    Given 'document', -> $(document)
    Given 'register menu', ->
      s = new Provider
      s.provide('track-context-menu', menuService)
      s.provide('player', service => @player)
      s.get('track-context-menu')

    When ->
      @registerMenu(@trackEl, => @track)
      @trackEl.find('button').click()
      w().delay(0)
    When -> $('.track-context-menu [data-action=play]').click()
    Then 'player', ({play})-> sinon.assert.calledOnce(play)
    Then 'player', ({play})-> sinon.assert.calledWith(play, @track)
    Then 'document', should.not.to.have.descendants('.track-context-menu')
