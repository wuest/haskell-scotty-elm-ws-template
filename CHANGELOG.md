# Revision history for scotty-elm-ws-template

## 0.3.0 -- 2024-05-05

* Updated aeson, text, bytestring, scotty, warp, websockets, and postgres
  constraints
* Breaking: Scotty types changed.

## 0.2.2 -- 2022-11-01

* Updated aeson, base, and mtl constraints

## 0.2.1 -- 2021-11-10

* Updated base and time constraints

## 0.2.0 -- 2021-10-12

* Minimum aeson version bumped to >= 2.0
* Restructured Types.elm to move websocket-specific types to Websocket.elm

## 0.1.1 -- 2021-03-17

* Revises versioning to drop the fourth number

## 0.1.0.4 -- 2020-04-12

* Bumps time dependency to allow building against 1.10

## 0.1.0.3 -- 2020-01-20

* Updates elm to 0.19.1

## 0.1.0.2 -- 2019-11-03

* Requires Network.Websockets 0.12+ to address deprecation of forkPingThread

## 0.1.0.1 -- 2019-06-30

* Bumps warp version to use up to 3.3
* Adds templating/wrappers for Persistent - supports postgres, mysql and sqlite
  out of the box
* Tested against Stack lts-13 (GHC 8.6), presumed to continue working with older
  builds of GHC

## 0.1.0.0 -- 2019-05-23

* Initial release.
* Built against Elm 0.19
* Tested against Stack lts-9 through lts-13 (GHC 8.0 through GHC 8.6)
