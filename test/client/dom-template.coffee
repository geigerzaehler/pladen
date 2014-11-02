define ['dom/template', 'support']
, (template, {should})->

  describe 'dom template', ->

    # Placeholder embedded in string
    When 'template', template('<p>--{{name}}--</p>')
    When -> @template.name('Mike')
    When '$el', -> $(@template.el)
    Then '$el', should.have.text('--Mike--')

    # Camel case the placeholder
    When 'template', template('<p>{{the-name}}</p>')
    When -> @template.theName('Mike')
    When '$el', -> $(@template.el)
    Then '$el', should.have.text('Mike')

    # Placeholder embedded in list of elements
    When 'template', template('<p><span>hi</span>{{name}}<input></p>')
    When -> @template.name('Mike')
    When '$el', -> $(@template.el)
    Then '$el', should.have.contain('Mike')
    Then '$el', should.have.descendants('span').and.contain('hi')
    Then '$el', should.have.descendants('input')

    # Deeply nested placeholder
    When 'template', template('<div><span></span><p>{{name}}</p></div>')
    When -> @template.name('Mike')
    When '$el', -> $(@template.el).find('p')
    Then '$el', should.have.text('Mike')


    describe 'set class', ->
      Given 'template', template('<div class="hey {{ho}}">')
      Given '$el', -> $(@template.el)

      When -> @template.ho(true)
      Then '$el', should.have.class('ho')

      When -> @template.ho(true)
      When -> @template.ho(false)
      Then '$el', should.not.have.class('ho')

      When -> @template.ho('yeah')
      Then '$el', should.have.class('hey').and.class('yeah')

      When -> @template.ho('yeah')
      When -> @template.ho('oh')
      Then '$el', should.not.have.class('yeah')
      Then '$el', should.have.class('oh')


    describe 'set attribute', ->
      Given 'template', template('<div><span {{hello}} data-name={{name}}>')
      Given '$el', -> $(@template.el).find('span')

      When -> @template.hello('world')
      Then '$el', should.have.attr('hello', 'world')

      When -> @template.name('Mike')
      Then '$el', should.have.attr('data-name', 'Mike')


    describe 'element slot', ->
      Given 'template', template('<div><div data-slot=slot><span>{{content}}')
      Given '$el', -> $(@template.el)

      When -> @template.slot($('<p>jo</p>')[0])
      Then '$el', should.have.descendants('p').and.have.text('jo')
      Then '$el', should.not.have.descendants('span')

      When -> @template.slot($('<p>jo</p>')[0])
      When -> @template.slot(null)
      Then '$el', should.have.descendants('span').and.text('{{content}}')

      Then 'template', should.not.have.property('content')
