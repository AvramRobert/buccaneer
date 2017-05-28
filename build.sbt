
val projectName = "buccaneer"
val localRepo = s"${Path.userHome.absolutePath}/Releases/$projectName"

name := projectName

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:implicitConversions"
)

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.11" % "7.2.8",
  "org.scalatest" % "scalatest_2.11" % "3.0.0",
  "org.scalacheck" % "scalacheck_2.11" % "1.13.4")

publishTo := Some(Resolver.file(s"${name.key.label}_$version", new File(localRepo)))