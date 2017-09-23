
name := "dotty-example-project"
description := "Example sbt project that compiles using Dotty"
version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-http" % "10.0.10",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.10",
  "com.typesafe.akka" %% "akka-stream" % "2.5.4",
  "com.typesafe.akka" %% "akka-actor"  % "2.5.4")
       