define ['support', 'views/search']
, ({should, eventStreamPromise}, {searchView})->

  describe 'search view', ->

    Given 'a searchView', searchView

    When.value 'search input', ->
      eventStreamPromise(@searchView.search)
    When 'I input "hey" into the search', ->
      @searchView.$el.find('input').val('hey').change()
    When 'search link', ->
      @searchView.$el.find('a').attr('href')
    Then 'search input', should.equal('hey')
    Then 'search link',  should.equal('#/q/hey')

    When.value 'downloadable value', ->
      eventStreamPromise(@searchView.downloadable)
    When 'I click the "downloadable" field', ->
      @searchView.$el.find('.search-downloadable').click()
    Then 'downloadable value', should.be.true
