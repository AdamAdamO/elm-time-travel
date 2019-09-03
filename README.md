# elm-time-travel

[![Build Status](https://travis-ci.org/jinjor/elm-time-travel.svg)](https://travis-ci.org/jinjor/elm-time-travel)

This is a fork of jinjor's elm-time-travel upgraded to work with elm 0.19.
An experimental debugger for Elm. See [DEMO](http://jinjor.github.io/elm-time-travel/)

## How to use

Just replace Browser.* with TimeTravel.*
So,
* Browser.sandbox become TimeTravel.sandbox
* Browser.element become TimeTravel.element
* Browser.document become TimeTravel.document
* Browser.application become TimeTravel.application

Also, because Debug.toString is no longer permitted in package, you need to 
pass it through a configuration object to TimeTravel.

Currently, this configuration object contains only 2 function, but may have 
other parameter in future.  See below for a sample of the configuration object.


```elm
import TimeTravel.Browser as TimeTravel

config = 
  { msgToString = Debug.toString
  , modelToString = Debug.toString
  }

main =
  -- Browser.element
  TimeTravel.element config
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
```

That's it!

## What is this library for?

Elm has [a great official debugger](http://elm-lang.org/blog/the-perfect-bug-report) from 0.18, but this debugger was born at 0.17! These two are focusing on slightly different things. The official one focuses on reproducing state and communicating between dev and QA people. This one, on the other hand, is more focusing on digging into problems that happen in runtime.

This library implements following features:

* Filtering Msgs
* Filtering Model
* Figure out how Msgs are chaining

And the ideas not implemented yet are:

* Watch partial Model and find Msgs that changes it
* Automatically save debugger state

So this library is a PoC of what the official debugger can potentially be in the future. Evan is also positive at this :)


## LICENSE

BSD3
