name := "avl-trees"

version := "1.0"

scalaVersion := "2.10.1"

scalacOptions += "-deprecation"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.0.M5b" % "test",
    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)
