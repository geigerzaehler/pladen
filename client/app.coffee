define [
  'router'
  'jquery'
  'fastclick'
  'when'
  'models/track'
  'models/album'
  'models/artist'
  'models/release'
  'views/base/view'
  'views/base/tab'
  'views/search'
  'views/splash_screen'
  'views/modal_manager'
  'views/album'
  'views/artists'
  'views/player'
  'track-context-menu'
  'global'
  'oneline'
  'bus'
  'bacon'
  'services'
  'player'
  'drag-track'
  'moment/lang/de'
  'es5-shim'
], (
  router
  $
  fastclick
  w
  Track
  Album
  Artist
  Release
  MyView
  TabView
  {searchView}
  splashScreen
  ModalManager
  {AlbumCollectionView, ReleaseCollection}
  ArtistsView
  PlayerView
  trackContextMenu
  {instance: global}
  oneline
  Bus
  Bacon
  {Provider}
  {player}
  dragTrack
)->


  fastclick.attach(document.body)

  class App
    start: ->
      splashScreen(w.try => @init()).done()

    init: ->
      @services = new Provider
      @services.provide(player)
      @services.provide(dragTrack)
      @services.provide(trackContextMenu)

      @bus = @vent = new Bus

      @tracks = new Track.Repository
      @singletonTracks = @tracks.singletons()
      @albums = new Album.Repository
      @artists = new Artist.Collection(@albums, @singletonTracks)
      @recentAlbums = @albums.recent()
      @recentReleases = new Release.Recent(@tracks, @albums)

      oneline(document)

      @modal = new ModalManager($('body'))
      # TODO Use services
      global.openNoAlbumDownload = @modal.openNoAlbumDownload.bind(@modal)
      global.openMessageDialog = @modal.openMessage.bind(@modal)
      MyView::app = global


      theSearchView = searchView()
      theSearchView.filter.assign global.search, 'dispatch'


      this.bus.on 'route:enter:search', (val)->
        theSearchView.$el.find('input').val(val)
        theSearchView.searchFragment.push(val)
        global.search.dispatch({search: val})

      @artistView = new ArtistsView(@artists, @services)
      @artistSearchView = $('<div>')
        .append(theSearchView.$el)
        .append(@artistView.$el)

      @recentReleasesView = new ReleaseCollection(@recentReleases, @services)
      @recentReleasesView.render()


      @tabs = new TabView
        artists: @artistSearchView
        recent:  @recentReleasesView.el
      @tabs.$el.addClass('content').appendTo('.container')

      @player = new PlayerView(@services.player)
      @player.$el.appendTo('.container')

      @bus.on 'route:enter:index', => @tabs.select('artists').done()
      @bus.on 'route:enter:new',   => @tabs.select('recent').done()

      router(@vent)
      w.all [
        @albums.fetchAll()
        @singletonTracks.fetchAll()
      ]

  return (window.a = new App)
