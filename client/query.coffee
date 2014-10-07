define ['tr', 'underscore'], (tr, {every, some})->
  track: (search)->
    if not search?
      return -> true

    patterns = for v in search.split(' ')
      pattern = RegExp(v, 'i')
      pattern.test.bind(pattern)

    return match = (track)->
      names = [track.title, track.artist]

      for name in names
        names.push tr(name)

      every patterns, (pattern)->
        some(names, pattern)

  # Return a function that matches an album and artist.
  #
  #   match = query('beatles pepper')
  #   match(album)
  #
  # Returns true if both of the words 'beatles' and 'pepper' are
  # contained in either the artist’s or the album’s name.  It
  # matches against the original and the transliteration of those
  # names.
  album: (search)->
    if not search?
      return -> true

    patterns = for v in search.split(' ')
      pattern = RegExp(v, 'i')
      pattern.test.bind(pattern)

    return match = (album)->
      names = [album.name, album.artist]

      for name in names
        names.push tr(name)

      every patterns, (pattern)->
        some(names, pattern)
