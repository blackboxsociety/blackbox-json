import sbt._
import sbt.Keys._

import bintray.Plugin._
import bintray.Keys._

object Build extends Build {

  val customBintraySettings = bintrayPublishSettings ++ Seq(
    packageLabels in bintray       := Seq("json"),
    bintrayOrganization in bintray := Some("blackboxsociety"),
    repository in bintray          := "releases"
  )

  val root = Project("root", file("."))
    .settings(customBintraySettings: _*)
    .settings(
      name                  := "blackbox-json",
      organization          := "com.blackboxsociety",
      version               := "0.1.0",
      scalaVersion          := "2.11.0",
      licenses              += ("MIT", url("http://opensource.org/licenses/MIT")),
      scalacOptions in Test ++= Seq("-Yrangepos"),
      resolvers             ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo),
      libraryDependencies   += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
      libraryDependencies   += "org.scalaz" %% "scalaz-core" % "7.0.6",
      libraryDependencies   += "org.specs2" %% "specs2" % "2.3.11" % "test"
    )

}