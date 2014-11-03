define ['filter', 'support'], ({album}, {should, expect})->

  describe 'album query', ->

    assertMatch = -> expect(@searchTest(@album)).to.be.true
    assertNoMatch = -> expect(@searchTest(@album)).to.be.false

    Given 'album', {}

    When -> @album.name = 'Abbey Road'
    When -> @album.artist = 'Beatles'
    When 'search test', -> album(search: 'beatles road')
    Then assertMatch

    When 'search test', -> album({})
    Then assertMatch


    describe 'download', ->
      Given 'album', {}

      When 'search test', -> album({})
      Then assertMatch

      When -> @album.downloadable = true
      When 'search test', -> album({})
      Then assertMatch

      When -> @album.downloadable = true
      When 'search test', -> album(downloadable: true)
      Then assertMatch

      When -> @album.downloadable = false
      When 'search test', -> album(downloadable: true)
      Then assertNoMatch

      When 'search test', -> album(downloadable: true)
      Then assertNoMatch

