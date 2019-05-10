## Haskell + Scotty + Elm + Websockets = <3

This repository puts forth a barebones skeleton application useful in cases
where a Haskell server and Elm 0.19 frontend are desired, particularly if
Websockets are needed.

### Usage

The contents of this repository can be built out of the box.  As such, to get
started on a new application:

 * Clone the repository
 * `rm -rf .git`
 * `git init`
 * Edit the `.cabal` file to reflect the application's information
 * `make`

The Makefile is generic, so updates to the cabal file are sufficient to be
reflected automatically in the make targets.

### Assumptions

The `Makefile` in particular makes a few assumptions:

 * `ghc` gets invoked directly - if invocation via `stack` is required, then it
   will be necessary to tweak it.
 * `elm` is in the path
 * `cabal` is expected to be of a sufficiently new version to support Nix-style
   builds (`v2-*` commands)
