define [
  'hasher'
  'crossroads'
], (
  hasher
  crossroads
) -> (vent)->
  router = crossroads.create()

  addRoute = (name, pattern)->
    route = router.addRoute pattern
    route.matched.add (args...)->
      vent.trigger("route:enter", name, args...)
      vent.trigger("route:enter:#{name}", args...)
    route.switched.add (args...)->
      vent.trigger("route:exit", name, args...)
      vent.trigger("route:exit:#{name}", args...)
    return route

  index  = addRoute 'index', ''
  new_   = addRoute 'new', 'new'
  search = addRoute 'search', 'q/{search}'

  search.matched.add (args...)->
    index.matched.dispatch(args)


  parseHash = (hash)=> router.parse(hash)
  hasher.changed.add parseHash
  hasher.initialized.add parseHash
  hasher.init()
