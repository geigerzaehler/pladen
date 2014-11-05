define ['services', 'support']
, (
  {Provider, globalProvider, service},
  {should, sinon}
)->

  describe 'services', ->

    Given 'a provider', -> new Provider
    Given -> globalProvider.services = {}

    # Simple service
    When -> @provider.provide('a', service -> 'no deps')
    When 'service', -> @provider.get('a')
    Then 'service', should.equal('no deps')

    # Dependent service
    When -> @provider.provide('a', service ['b', 'c'], (b, c)-> [b,c])
    When -> @provider.provide('b', service -> 'b')
    When -> @provider.provide('c', service -> 'c')
    When 'service', -> @provider.get('a')
    Then 'service', should.deep.equal(['b', 'c'])

    # Named service
    When -> @provider.provide(service 'a', ['b'], (b)-> b)
    When -> @provider.provide(service 'b', -> 'b')
    When 'service', -> @provider.get('a')
    Then 'service', should.equal('b')

    # Global services
    When -> service 'a', -> 'this is a'
    When 'service', -> globalProvider.get('a')
    Then 'service', should.equal('this is a')

    # Initialize service once
    When 'init', -> sinon.spy(-> 'a')
    When -> @provider.provide service('a', @init)
    When ->
      @provider.get('a')
      @provider.get('a')
      @provider.get('a')
    Then 'init', sinon.assert.calledOnce

    # Child provider
    Given 'child', -> @provider.extend()
    When -> @child.provide service('a', ['b'], (b)-> b)
    When -> @provider.provide service('b', -> 'b')
    When 'service', -> @child.get('a')
    Then 'service', should.equal('b')
