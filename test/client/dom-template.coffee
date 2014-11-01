define ['dom/template', 'support']
, (template, {should})->

  describe.only 'dom template', ->

    When 'template', template('<p> {{name}} </p>')
    When -> @template.name('Mike')
    When '$el', -> $(@template.el)
    Then '$el', should.have.text('Mike')

    When 'template', template('<p><span></span>{{name}}</p>')
    Then 'template', should.not.have.property('name')

    When 'template', template('<p>!!{{name}}!!</p>')
    Then 'template', should.not.have.property('name')

    When 'template', template('<div><span></span><p>{{name}}</p></div>')
    When -> @template.name('Mike')
    When '$el', -> $(@template.el).find('p')
    Then '$el', should.have.text('Mike')

    When 'template', template('<div><span data-name={{name}}>')
    When -> @template.name('Mike')
    When '$el', -> $(@template.el).find('span')
    Then '$el', should.have.attr('data-name', 'Mike')

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
