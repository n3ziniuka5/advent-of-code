Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

name := "advent-of-code-2025"

scalacOptions ++= List(
  "-encoding",
  "utf8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wnonunit-statement",
  "-Wvalue-discard",
  "-experimental"
)

//    testing 1343e54

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-collection-contrib"   % "0.4.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
  "dev.zio"                %% "zio-prelude"                % "1.0.0-RC43",
  "com.lihaoyi"            %% "requests"                   % "0.9.0",
  "org.apache.commons"      % "commons-lang3"              % "3.20.0",
  "dev.zio"                %% "zio-test"                   % "2.1.23" % Test,
  "dev.zio"                %% "zio-test-sbt"               % "2.1.23" % Test
)
