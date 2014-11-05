# Collects all templates so we can reference them as properties on an
# object.
define [
  'dom/template'
, 'text!templates/search.html'
, 'hgn!templates/player'
, 'hgn!templates/artist-album'
, 'hgn!templates/artist-track'
, 'hgn!templates/track-release'
, 'hgn!templates/track-context-menu'
, 'text!templates/drag-track.mustache'
, 'hgn!templates/dialogs/message'
], (
  domTemplate
  search
  player
  artistAlbum
  artistTrack
  trackRelease
  trackContextMenu
  dragTrackSrc
  messageDialog_
)->
  dragTrack = -> domTemplate(dragTrackSrc)
  messageDialog = (msg)-> messageDialog_ {msg}

  {search, artistAlbum, artistTrack, trackRelease
    player, trackContextMenu, dragTrack, messageDialog}
