Erlang PerfChk - website performance checker
============================================

[![Build Status](https://travis-ci.com/paweldudzinski/perfchk.svg?branch=master)](https://travis-ci.com/paweldudzinski/perfchk)
[![Erlang Versions][erlang versions badge]][erlang]
[![License](https://img.shields.io/badge/License-Apache%202.0-9cf.svg)](https://opensource.org/licenses/Apache-2.0)
[![Latest Release][release badge]][release]
[![Last Commit][commit badge]][commit]

Check your website performance using your SauceLabs account and compare metrics with tests done in the past.
* Setup with your SauceLabs username and access key
* Set your test name
* Run perfchk as a part of your CI/CD

Before compiling edit `perfchk.app.src.template`, use your SauceLabs credentials and rename this file to `perfchk.app.src`

### Build

    $ rebar3 compile

### Run

    $ ./run.sh


### Test

    $ rebar3 eunit




### Output example

```
$ ./run.sh
Starting remote session as papaduda
Checking performance of http://www.papaduda.pl
Parallel tests launched (5)....

Page load       : 1296.0 ms
Requests        : 11.0
Page weight     : 219.444 KB
Speed index     : 1155.4 ms

Performance check: pass
```

<!-- Links (alphabetically) -->
[commit]: https://github.com/paweldudzinski/perfchk/commit/HEAD
[erlang]: http://www.erlang.org
[eunit stdout]: http://erlang.org/doc/apps/eunit/chapter.html#Running_EUnit
[release]: https://github.com/paweldudzinski/perfchk/releases/latest

<!-- Badges (alphabetically) -->
[commit badge]: https://img.shields.io/github/last-commit/paweldudzinski/perfchk.svg
[erlang versions badge]: https://img.shields.io/badge/erlang-18.0%20to%2021.3-orange.svg
[release badge]: https://img.shields.io/github/release/paweldudzinski/perfchk.svg
