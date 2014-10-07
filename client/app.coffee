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
  'views/player'
  'global'
  'oneline'
  'bus'
  'moment/lang/de'
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
  SearchView
  AppLoaderView
  ModalManager
  {AlbumCollectionView, ReleaseCollection}
  ArtistsView
  Player
  {instance: global}
  oneline
  Bus
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
        oneline(document)

        @modal = new ModalManager($('body'))
        # TODO rename and abstract
        global.openNoAlbumDownload = @modal.openNoAlbumDownload.bind(@modal)
        global.openMessageDialog = @modal.openMessage.bind(@modal)
        MyView::app = global

        @artistSearchView = new BagView(
          new SearchView(@bus),
          new ArtistsView(@artists, @vent)
        )
        # @albumsView = new AlbumCollectionView(@recentAlbums)
        @albumsView = new ReleaseCollection(@recentReleases)

        @player = new Player(@tracks)

        @tabs = new ContentView
          artists: @artistSearchView
          albums:  @albumsView
        $('.container').append(@tabs.el, @player.el)

        @bus.on 'route:enter:index', => @tabs.select('artists').done()
        @bus.on 'route:enter:new',   => @tabs.select('albums').done()

        router(@vent)
      .then => w.all [
        @albums.fetchAll()
        @singletonTracks.fetchAll()
      ]

  return (window.a = new App)
