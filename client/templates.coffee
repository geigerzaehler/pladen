# Collects all templates so we can reference them as properties on an
# object.
define [
  'dom/template'
, 'text!templates/search.mustache'
, 'hgn!templates/player'
, 'hgn!templates/artist-album'
, 'hgn!templates/artist-track'
, 'hgn!templates/track-release'
, 'hgn!templates/track-context-menu'
, 'text!templates/drag-track.mustache'
], (
  domTemplate
  search
  player
  artistAlbum
  artistTrack
  trackRelease
  trackContextMenu
  dragTrackSrc
)->
  dragTrack = -> domTemplate(dragTrackSrc)
  return {search, artistAlbum, artistTrack, trackRelease
    player, trackContextMenu, dragTrack}
