define ['support', 'views/search']
, ({should, eventStreamPromise}, {searchView})->

  describe 'search view', ->

    Given 'a searchView', searchView
    Given.value 'filter', ->
      eventStreamPromise(@searchView.filter)

    When 'I input "hey" into the search', ->
      @searchView.$el.find('input').val('hey').change()
    When 'search link', ->
      @searchView.$el.find('a').attr('href')
    Then 'filter', should.have.property('search').that.equals('hey')
    Then 'search link',  should.equal('#/q/hey')

    When 'I click the "downloadable" field', ->
      @searchView.$el.find('.search-downloadable').click()
    Then 'filter', should.have.property('downloadable').that.is.true
