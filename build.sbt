name := "scala-css-selectors"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

javacOptions ++= Seq("-encoding", "UTF-8")

scalacOptions += "-Xexperimental"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3" % Provided,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3" % Provided,
  "org.scalatest" %% "scalatest" % "2.2.4" % Test,
  "org.scalacheck" %% "scalacheck" % "1.12.2" % Test
)

autoAPIMappings := true // This requires the m:properties__info.apiURL property to be defined for each dependency in their Ivy XML
