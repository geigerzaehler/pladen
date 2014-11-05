define ['dialogs', 'support']
,      (dialogs, {should, sinon, expect, w, $})->

  describe.only 'dialogs', ->

    Given 'dialogs', -> dialogs.init()
    Given '$dialogs', -> @dialogs.$el
    Given -> @dialogs.message('Hello World')

    Then '$dialogs', should.contain('Hello World')

    When -> @$dialogs.find('button[data-action=close]').click()
    Then '$dialogs', should.not.contain('Hello World')


    afterEach -> $('.dialogs').remove()
