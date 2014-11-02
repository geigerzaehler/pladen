# Collects all templates so we can reference them as properties on an
# object.
define [
  'dom/template'
, 'text!templates/search.mustache'
, 'hgn!templates/player'
, 'hgn!templates/track-context-menu'
, 'text!templates/drag-track.mustache'
], (
  domTemplate
  search
  player
  trackContextMenu
  dragTrackSrc
)->
  dragTrack = -> domTemplate(dragTrackSrc)
  return {search, player, trackContextMenu, dragTrack}
