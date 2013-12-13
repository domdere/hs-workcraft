# Workcraft Service [![Build Status](https://travis-ci.org/domdere/hs-workcraft.png?branch=master)](https://travis-ci.org/domdere/hs-workcraft)

Workcraft Service

An blogging backend service to learn about and/or experiment with **Yesod**.

## Building the project

The project must be "configured" at least once everytime `workcraft-service.cabal` changes, this can be done with:

    cabal configure

If you wish to run the unit tests you will have to run:

    cabal configure --enable-tests

Then finally build it with:

    cabal build

See `cabal build --help` for more build options.

## Yesod Development

You first need to install the **Yesod Development Platform** like so (Preferably **before** you run `cabal install` on this project):

    cabal install yesod-platform
    cabal install yesod-bin

To run the development server run

    $ yesod devel

From the same directory as the `workcraft-service.cabal` file

See [**here**] [yesod-home] for more information on **Yesod**.

## Developing the Project

See the [**Development Guide**] [development-guide].

## Development: Cabal Dependency Hell?

Cabal's great, but when you are developing a few different projects with their own dependency chains, sometimes installing all your libraries to the same place causes problems,

Consider trying [`cabal-dev`] [cabal-dev]. In terms of using it, all thats required is replacing `cabal` with `cabal-dev` in all the above command lines.

It will download and install all the dependencies for your project and install them in a `cabal-dev/` directory in your project directory, and they will only be used for this project.

## Running Unit Tests

**After** running `cabal build`, you can run the unit tests with the command:

    cabal test

## Adding Unit tests

Unit tests are written with [**doctest**] [doctest-github], for instructions on how to add unit tests
see the **doctest** [**User Guide**] [doctest-userguide].

Currently only files in the `src/` directory are searched for tests, it is assumed that the code in `main/`
is a thin layer of code that uses modules from `src/`.
[doctest-github]: https://github.com/sol/doctest-haskell "sol/doctest-haskell on GitHub.com"
[doctest-userguide]: https://github.com/sol/doctest-haskell/blob/master/README.markdown#usage "doctest Usage Guide"
[cabal-dev]: https://github.com/creswick/cabal-dev "creswick/cabal-dev on GitHub.com"
[yesod-home]: http://www.yesodweb.com/ "Yesod Web Application Framework"
[development-guide]: docs/DevelopmentGuide/README.md "Development Guide"
