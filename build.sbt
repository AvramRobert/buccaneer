name := "buccaneer"

version := "1.0"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.11" % "7.1.4",
  "org.scalatest" % "scalatest_2.11" % "3.0.0",
  "org.scalacheck" % "scalacheck_2.11" % "1.13.4")