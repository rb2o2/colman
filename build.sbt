lazy val commonSettings = Seq(
  name := "colman",
  description := "Personal library/collection management server built on akka-http",
  version := "0.2-SNAPSHOT",
  scalaVersion := "2.11.8")

lazy val colmanCore = (project in file(".")).settings(
  commonSettings,
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.10",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test"
  )
)

lazy val colmanSwing = (project in file("frontend/swing"))
  .settings(commonSettings)
  .dependsOn(colmanCore)


lazy val colmanCli = (project in file("frontend/cli"))
  .settings(commonSettings)
  .dependsOn(colmanCore)

lazy val android = (project in file("android"))
  .settings(commonSettings)
  .dependsOn(colmanCore).enablePlugins(AndroidApp)

excludeFilter :=
  FileFilter.globFilter("*WebServer*")

