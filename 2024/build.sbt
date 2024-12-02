Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.2"

name := "advent-of-code-2024"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-collection-contrib"   % "0.4.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "dev.zio"                %% "zio-prelude"                % "1.0.0-RC35",
  "com.lihaoyi"            %% "requests"                   % "0.9.0",
  "dev.zio"                %% "zio-test"                   % "2.1.13" % Test,
  "dev.zio"                %% "zio-test-sbt"               % "2.1.13" % Test
)
