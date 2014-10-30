define ['services', 'support']
, (
  {ServiceProvider, service},
  {should}
)->

  describe 'services', ->

    Given 'a provider', -> new ServiceProvider

    When -> @provider.provide('a', service -> 'no deps')
    When 'service', -> @provider.get('a')
    Then 'service', should.equal('no deps')

    When -> @provider.provide('a', service ['b', 'c'], (b, c)-> [b,c])
    When -> @provider.provide('b', service -> 'b')
    When -> @provider.provide('c', service -> 'c')
    When 'service', -> @provider.get('a')
    Then 'service', should.deep.equal(['b', 'c'])
