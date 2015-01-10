# Haskbot

Or *Haskbot 2: Haskell Strikes Back*. Haskbot is (still) a chatbot for the
[Slack](https://slack.com) group chat application. In this revision of
Haskbot the authors have focused on simultaneously simplifying the
architecture and providing plugin authors with more flexibility in their
plugin code.

The general approach is to have plugin authors write plugins using a
friendly DSL, still allowing the full power of Haskell, and then hand that
script off to Haskbot to run.

## Background and implementation

The core idea is to expose a
[DSL](http://en.wikipedia.org/wiki/Domain-specific_language) based upon the
idea of a [free
monad](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html).
