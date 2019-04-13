Erlang PerfChk - website performance checker
============================================

[![Build Status](https://travis-ci.com/paweldudzinski/perfchk.svg?branch=master)](https://travis-ci.com/paweldudzinski/perfchk)

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
