define ['views/player2', 'player', 'support', 'support/audio', 'support/events']
, ({Player}, {player}, {should, promiseCall}, Audio, {DragEvent})->

  describe 'player', ->

    before -> Audio.mock(window)
    after  -> Audio.restore()

    describe 'playlist', ->
      Given 'a player', -> new Player(player.init())
      And 'the playlist', -> @player.$el.find('.player-playlist-window')

      Then 'playlist', should.have.attr('aria-hidden')

      When -> @player.$el.find('.player-toggle-playlist').click()
      Then 'playlist', should.not.have.attr('aria-hidden')

    describe 'playing and pausing', ->

      dropTrack = ->
        DragEvent('drop', 'application/x-play-track': @track)
        .dispatch(@dropTarget)


      Given 'a player', -> new Player(player.init())
      And 'player element', -> @player.$el
      And 'player control', -> @playerElement.find('.player-control-play')
      And 'drop target', -> @playerElement.find('.player-drop-target')[0]
      And 'track', id: 1, title: 'Song 2', artist: 'Blur', length: 200


      When.value 'audio play', -> promiseCall Audio.prototype, 'play'
      When dropTrack
      Then 'audio play', should.be.true
      Then 'player element', should.have.class('playing')
      Then 'player control', should.have.attr('aria-label', 'Pause')
      Then 'player element', should.contain('Blur')
      Then 'player element', should.contain('Song 2')
      Then 'player element', should.contain('3:20')


      When dropTrack
      When -> @playerElement.find('.player-control-play').click()
      Then 'player element', should.not.have.class('playing')
      Then 'player control', should.have.attr('aria-label', 'Play')

      describe 'progress', ->

        Given 'audio', -> Audio.instance()
        And 'play progress', -> @playerElement.find('.player-progress-play')

        When -> @audio.setProgress(30, 60)
        Then 'play progress', should.have.attr('value', '0.5')
