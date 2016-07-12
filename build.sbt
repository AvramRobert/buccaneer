name := "buccaneer"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalaz" % "scalaz-core_2.11" % "7.1.4",
  "org.scalatest" % "scalatest_2.11" % "3.0.0-M7",
  "org.scalaz.stream" % "scalaz-stream_2.11" % "0.8"
)