import sbt.Keys.resolvers

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "typed-formula",
    libraryDependencies ++= Dependencies.root,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1")
  )
  .aggregate(macros)
  .dependsOn(macros)

lazy val macros = (project in file("macros"))
  .settings(commonSettings: _*)
  .settings(
    name := s"${Settings.name}-macros",
    libraryDependencies ++= Dependencies.macros
  )

lazy val commonSettings = Seq(
  organization := Settings.organization,
  scalaVersion := Versions.scala,
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-encoding", "UTF-8",
    "-Xplugin-require:macroparadise",
    "-Ypartial-unification"
  ),
  resolvers ++= List(
    "Sbt plugins"                   at "https://dl.bintray.com/sbt/sbt-plugin-releases",
    "Sonatype Releases"             at "http://oss.sonatype.org/content/repositories/releases",
    "Maven Central Server"          at "http://repo1.maven.org/maven2",
    "TypeSafe Repository Releases"  at "http://repo.typesafe.com/typesafe/releases/",
    "TypeSafe Repository Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
    "Scalameta"                     at "http://dl.bintray.com/scalameta/maven"
  ),
  addCompilerPlugin(Library.scalametaParadise)
)