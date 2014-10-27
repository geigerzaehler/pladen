# Collects all templates so we can reference them as properties on an
# object.
define [
  'hgn!templates/search'
, 'hgn!templates/player']
, (
  search
  player
)->
  return {search, player}
