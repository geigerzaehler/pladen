testFiles = []

TEST_INCLUDE = new RegExp('^/base/test/client')
TEST_EXCLUDE = new RegExp('^/base/test/client/support')

for file in Object.keys(window.__karma__.files)
  if file.match(TEST_INCLUDE) and not file.match(TEST_EXCLUDE)
    testFiles.push(file)


require.config
  baseUrl: '/base/client'
  paths:
    chai: '../node_modules/chai/chai'
    'chai-builder': '../node_modules/chai-builder/index'
    'chai-jquery': '../node_modules/chai-jquery/chai-jquery'
    'mocha-when-then': '../node_modules/mocha-when-then/dist/browser-bundle'
    support: '../test/client/support'


require ['mocha-when-then'], ->
  mocha.ui('when-then')
  require testFiles, ->
    window.__karma__.start()
