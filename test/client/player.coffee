define ['player', 'support', 'support/audio', 'support/events']
, ({player}, {should, eventStreamPromise}, Audio, {DragEvent})->

  describe 'player', ->

    before -> Audio.mock(window)
    after  -> Audio.restore()

    firstTrack = { id: 1, title: 'Song 2', artist: 'Blur', length: 200 }
    secondTrack = { id: 2, title: 'Song 3', artist: 'Blur', length: 200 }

    Given 'player', -> player.init()
    And 'audio', -> Audio.instance()

    When.value 'second played track', ->
      eventStreamPromise(@player.currentTrack.skip(2))
    When -> @player.play(firstTrack)
    And  -> @player.playlist.push(secondTrack)
    And  -> @audio.end()
    Then 'second played track', should.deep.equal(secondTrack)
