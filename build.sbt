
name := "Colman - simple collection manager"
description := "Personal library/collection management server built on akka-http"
version := "0.1"

//scalaVersion := "2.12.3"
scalaVersion := "0.5.0-RC1"

excludeFilter :=
  FileFilter.globFilter("*Marshallers*") ||
  FileFilter.globFilter("*WebServer*") ||
  FileFilter.globFilter("*DB*")

libraryDependencies ++= Seq(
//  "com.typesafe.akka" %% "akka-http" % "10.0.10",
//  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.10",
//  "com.typesafe.akka" %% "akka-stream" % "2.5.4",
//  "com.typesafe.akka" %% "akka-actor"  % "2.5.4",
  "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"
//  ,"com.typesafe.slick" %% "slick" % "3.2.1",
//  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1",
//  "org.slf4j" % "slf4j-nop" % "1.6.4"
)
