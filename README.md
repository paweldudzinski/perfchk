Erlang PerfChk - website performance checker
============================================

[![Build Status](https://travis-ci.com/paweldudzinski/perfchk.svg?branch=master)](https://travis-ci.com/paweldudzinski/perfchk)
[![Erlang Versions][erlang versions badge]][erlang]
[![License][license badge]][license]
[![Latest Release][release badge]][release]
[![Last Commit][commit badge]][commit]

Check your website performance using your SauceLabs account and compare metrics with tests done in the past.
* Setup with your SauceLabs username and access key
* Set your test name
* Run perfchk as a part of your CI/CD

Build
-----

    Edit `perfchk.app.src.template`, use your SauceLabs credentials and rename this file to `perfchk.app.src`
    $ rebar3 compile

Run
-----

    $ ./run.sh


Test
-----

    $ rebar3 eunit


<!-- Links (alphabetically) -->
[commit]: https://github.com/paweldudzinski/perfchk/commit/HEAD
[erlang]: http://www.erlang.org
[eunit stdout]: http://erlang.org/doc/apps/eunit/chapter.html#Running_EUnit
[license]: ./LICENSE
[release]: https://github.com/paweldudzinski/perfchk/releases/latest

<!-- Badges (alphabetically) -->
[commit badge]: https://img.shields.io/github/last-commit/paweldudzinski/perfchk.svg?style=flat-square
[erlang versions badge]: https://img.shields.io/badge/erlang-20.0%20to%2021.3-blue.svg?style=flat-square
[license badge]: https://img.shields.io/github/license/paweldudzinski/perfchk.svg?style=flat-square
[release badge]: https://img.shields.io/github/release/paweldudzinski/perfchk.svg?style=flat-square
