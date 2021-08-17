name := "nano-symex"

version := "0.1"

scalaVersion := "2.13.5"

scalacOptions ++= Seq(
    "-deprecation",
    //"-Xfatal-warnings",
    "-unchecked",
    "-Xlint",
    "-Xelide-below", "INFO",
    "-feature",
    "-opt-inline-from:**",
    "-opt:l:method",
    "-opt:l:inline",
    "-Ywarn-dead-code",
    "-Ywarn-unused"
)

run / fork := true

Global / cancelable := true
