Black Box Json
==============

[![Build Status](https://travis-ci.org/blackboxsociety/blackbox-json.svg?branch=master)](https://travis-ci.org/blackboxsociety/blackbox-json)

This is a very simple JSON parser with minimal dependencies. The parser itself is
only about 60 lines of code and the code to print json is much less.


To use add the following to your Build.scala settings
```scala
resolvers += "Black Box Society Repository" at "http://dl.bintray.com/blackboxsociety/releases",
libraryDependencies += "com.blackboxsociety" %% "blackbox-json" % "0.1.0"
```
