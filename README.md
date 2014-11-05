Pladen
======

This is an application to access your [beets][] library through the
browser.

You can run the server with a prepopulated beets database.

```
$ cabal run -- --config fixtures/pladen.conf
```

This will use the data from the `fixtures` folder to fire up a server
at `http://localshost:3000`.


Building
--------

To build the server and the web application your first need to setup
your build system with `make build-deps`. You can then run `make all`
to build the application.


Development
-----------

The code of this project consists of three parts.

* The web server application, written in Haskell, and contained in the
  `Pladen` directory.

* The client-side JavaScript application. It is written in TypeScript
  and CoffeeScript and contained in the `client` directory. It also
  includes some vendor modules through bower.

* HTML and Stylesheets. There is only one HTML file, `index.html`. The
  stylesheets are build with SASS and the source files contained in
  `styles`.

To get a deeper understanding of how the project is structured and
built, check out the Makefile.


### Client

This is an overview of all the client files and what they do.

* `services` Provides a service constructor and service provider.
  Implementation of the depency injector pattern.

* `drag-track` (service) Make DOM elements that represent tracks
  draggable so that they can interact with the rest of the application.

* `track-context-menu` (service) Provide context menu to play tracks.

* `templates` Exposes a collection of functions that generate strings
  or DOM elements from data.

* `dom` Provides various abstractions on top of DOM Elements. Includes
  *DOM signals* (instead of events) and FRP *drag and drop stream*.

* `filter` Create functions that test if a model satisfier a given
  filter.

The following modules should be replaced

* `global`
* `observer`
* `emitter`
* `emitter_observer`
* `bus`
* `router`


[beets]: https://github.com/sampsyo/beets
