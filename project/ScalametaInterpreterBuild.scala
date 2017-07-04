import sbt.Keys._
import sbt._

object ScalametaInterpreterBuild {
  lazy val commonSettings = Seq(
    organization := "org.scalameta",
    scalaVersion := "2.12.2",
    scalacOptions := Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-unused-import",
      "-Xfuture",
      "-Xexperimental",
      "-Xdev"
    ),
    scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
    libraryDependencies ++= Seq(
      "ch.qos.logback"             % "logback-classic" % "1.1.7",
      "com.typesafe.scala-logging" %% "scala-logging"  % "3.5.0",
      "org.scalatest"              %% "scalatest"      % "3.0.1" % Test
    )
  )

  lazy val interpreter = Project(id = "interpreter", base = file("interpreter"))
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scalameta" %% "scalameta" % "1.8.0",
        "org.scalameta" %% "contrib" % "1.8.0"
      )
    )

  lazy val root = Project(id = "scalameta-interpreter", base = file("."))
    .settings(commonSettings)
    .aggregate(interpreter)
}
