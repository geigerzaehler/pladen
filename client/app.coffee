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
  'views/base/bag'
  'views/search'
  'views/app_loader'
  'views/modal_manager'
  'views/album'
  'views/artists'
  'views/player2'
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
  BagView
  {searchView}
  AppLoaderView
  ModalManager
  {AlbumCollectionView, ReleaseCollection}
  ArtistsView
  {Player: PlayerView}
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

  class ContentView extends TabView
    elementTemplate: '<ol class=content>'
  Object.defineProperty ContentView.prototype, 'elementTemplate',
    value: '<ol class=content>'

  class App
    constructor: ->
      @bus = @vent = new Bus

      @tracks = new Track.Repository
      @singletonTracks = @tracks.singletons()
      @albums = new Album.Repository
      @artists = new Artist.Collection(@albums, @singletonTracks)
      @recentAlbums = @albums.recent()
      @recentReleases = new Release.Recent(@tracks, @albums)


    start: ->
      appLoader = new AppLoaderView($('.app-loader'))
      @_start().done(
        => setTimeout (=> appLoader.finish()), 10
      , (e)=>
        appLoader.fail()
        throw e
      )

    _start: ->
      w.try =>
        @services = services = new Provider
        services.provide('player', player)
        services.provide('drag-track', dragTrack)
        services.provide('track-context-menu', trackContextMenu)

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

        @artistView = new ArtistsView(@artists, services)
        @artistSearchView = $('<div>')
          .append(theSearchView.$el)
          .append(@artistView.$el)

        @recentReleasesView = new ReleaseCollection(@recentReleases, services)
        @recentReleasesView.render()


        @tabs = new TabView
          artists: @artistSearchView
          recent:  @recentReleasesView.el
        @tabs.$el.addClass('content').appendTo('.container')

        @player = new PlayerView(services.get('player'))
        @player.$el.appendTo('.container')

        @bus.on 'route:enter:index', => @tabs.select('artists').done()
        @bus.on 'route:enter:new',   => @tabs.select('recent').done()

        router(@vent)
      .then => w.all [
        @albums.fetchAll()
        @singletonTracks.fetchAll()
      ]

  return (window.a = new App)
