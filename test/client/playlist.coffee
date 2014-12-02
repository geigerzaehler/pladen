define ['player', 'support', 'utils/splice', 'support/audio']
, ({player}, {should, eventStreamPromise}, splice, Audio)->

  describe 'playlist', ->

    before -> Audio.mock(window)
    after  -> Audio.restore()

    firstTrack = { id: 1, title: 'Song 2', artist: 'Blur', length: 200 }
    secondTrack = { id: 2, title: 'Song 3', artist: 'Blur', length: 200 }

    Given 'playlist', -> player.init().playlist

    When.value 'current track', -> eventStreamPromise @playlist.current
    When -> @playlist.queue(firstTrack)
    Then 'current track', should.equal(firstTrack)

    When.value 'queue', ->
      eventStreamPromise @playlist.changes.scan([], splice.apply)
    When ->
      @playlist.queue(firstTrack)
      @playlist.queue(secondTrack)
    Then 'queue', should.be.deep.equal([secondTrack])

    When.value 'next track', -> eventStreamPromise @playlist.current.skip(1)
    When ->
      @playlist.queue(firstTrack)
      @playlist.queue(secondTrack)
      @playlist.next()
    Then 'next track', should.equal(secondTrack)

    When.value 'no track', -> eventStreamPromise @playlist.current.skip(1)
    When ->
      @playlist.queue(firstTrack)
      @playlist.next()
    Then 'no track', should.be.undefined

    When.value 'set track', -> eventStreamPromise @playlist.current.skip(1)
    When ->
      @playlist.queue(firstTrack)
      @playlist.set(secondTrack)
    Then 'set track', should.equal(secondTrack)
