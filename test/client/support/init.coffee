testFiles = []
karma = window.__karma__

TEST_INCLUDE = new RegExp('^/base/test/client')
TEST_EXCLUDE = new RegExp('^/base/test/client/support')

for file in Object.keys(window.__karma__.files)
  if file.match(TEST_INCLUDE) and not file.match(TEST_EXCLUDE)
    testFiles.push(file)


require.config
  baseUrl: '/base/client'
  paths: karma.config.requirePaths


require ['mocha-when-then'], ->
  mocha.ui('when-then')
  require testFiles, -> karma.start()
