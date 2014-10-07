define [
  'emitter'
  'bus_client'
],(
  EventEmitter
  BusClient
)->

  # EventEmitter with extra methods to create and connect
  # EmitterObservers.
  class Bus extends EventEmitter

    # Connect the BusClient 'obj.bus' to this bus.
    connect: (obj)->
      if not obj.bus?.observe?
        throw Error("#{obj} doesn't have a bus client")
      obj.bus.connect(this)

    # Set 'obj.bus' to a BusClient and connect it to this bus.
    #
    # If called as a class method with 'Bus::client()' it
    # creates a client that is not yet connected.
    client: (obj)->
      if this != Bus.prototype
        target = this
      obj.bus = new BusClient(obj, target)
