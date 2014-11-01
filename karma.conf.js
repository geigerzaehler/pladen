// Karma configuration
// Generated on Wed Sep 24 2014 19:38:16 GMT+0200 (CEST)

var path = require('path');
var _ = require('underscore');
var values = _.values;
var extend = _.extend;

var base = process.cwd();

var nodeModules = nodeModulePaths({
  // Test libraries
  'mocha-when-then': './dist/browser-bundle',
  'chai': './chai',
  'chai-builder': true,
  'chai-jquery': true,
  'sinon': './pkg/sinon',

  // Support and Mock
  'event-target': './build/event-target.amd'
});

var amdModules = extend({
  support: 'test/client/support'
}, nodeModules);


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

      'test/client/support/init.coffee',

      // Client source files
      {pattern: 'client/**/*.js', included: false},
      {pattern: 'client/**/*.mustache', included: false},

      // Tests
      {pattern: 'test/client/**/*.coffee', included: false}
    ].concat(values(nodeModules).map(amdPattern)),

    client: {
      requirePaths: mapValues(amdModules, amdPath),
      captureConsole: true,
      useIframe: true,
    },


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
    logLevel: config.LOG_WARN,


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


function mapValues(object, iteratee) {
  var mapped = {};
  _.each(object, function(value, key) {
    mapped[key] = iteratee(value);
  });
  return mapped;
}


function amdPattern(file) {
  return {pattern: file, included: false};
}


function amdPath(file) {
  return path.relative(path.join(base, 'client'), file).replace(/\.js$/, '');
}


function nodeModulePaths(nodeModules) {
  var paths = {};
  for (var name in nodeModules) {
    var main = nodeModules[name];
    if (main === true)
      main = require.resolve(name);
    else
      main = require.resolve(path.join(name, main));
    paths[name] = main;
  }
  return paths;
}
