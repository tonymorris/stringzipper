import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  override lazy val settings = super.settings ++
        Seq(resolvers := Seq(
          "mth.io snapshots"  at "http://repo.mth.io/snapshots"
        , "mth.io releases"  at "http://repo.mth.io/releases"
        , "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
        , "releases"  at "http://oss.sonatype.org/content/repositories/releases"
        ))

  val stringzipper = Project(
    id = "stringzipper"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "stringzipper"
    , organization := "net.tmorris"
    , version := "1.0-SNAPSHOT"
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.9" % "test" withSources
      )
    )
  )

  val example = Project(
    id = "example"
  , base = file("example")
  , dependencies = Seq(stringzipper)
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "example"
    , organization := "net.tmorris"
    , version := "1.0"
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.9" % "test" withSources
      )
    )
  )
}
