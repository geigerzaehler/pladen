// Karma configuration
// Generated on Wed Sep 24 2014 19:38:16 GMT+0200 (CEST)

var path = require('path');

module.exports = function(config) {
  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',


    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['mocha', 'requirejs'],


    // list of files / patterns to load in the browser
    files: [
      // Bower modules as AMD
      'assets/boot.dev.js',


      // Mocha Given When Then UI
      {pattern: 'node_modules/mocha-when-then/dist/browser-bundle.js', included: false},
      {pattern: 'node_modules/chai-builder/index.js', included: false},

      // Start the test suite with requirejs
      'test/client/support/init.coffee',

      // Chai Expectations
      {pattern: 'node_modules/chai/chai.js', included: false},

      // Client source files
      {pattern: 'client/**/*.js', included: false},
      {pattern: 'client/**/*.mustache', included: false},

      // Tests
      {pattern: 'test/client/**/*.coffee', included: false}
    ],


    // list of files to exclude
    exclude: [
    ],


    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
      '**/*.coffee': ['coffee'],
    },

    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['progress', 'notify'],

    notifyReporter: {
      reportSuccess: false
    },


    // web server port
    port: 9876,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,


    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
    browsers: ['PhantomJS'],


    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
    singleRun: false
  });
};
