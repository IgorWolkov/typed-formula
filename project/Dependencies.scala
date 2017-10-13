import sbt._

object Settings {
  val organization          = "karazinscalausersgroup"
  val name                  = "typed-formula"
  val version               = "0.1.0-SNAPSHOT"
}

object Versions {
  val scala                 = "2.12.3"

  val scalameta             = "1.8.0"
  val scalametaParadise     = "3.0.0-M10"

  val shapeless             = "2.3.2"
  val scalacheck            = "1.13.4"
}

object Library {
  lazy val shapeless             = "com.chuusai"     %% "shapeless"        % Versions.shapeless
  lazy val scalacheck            = "org.scalacheck"  %% "scalacheck"       % Versions.scalacheck % "test"

  lazy val scalaReflect          = "org.scala-lang" %    "scala-reflect"   % Versions.scala
  lazy val scalameta             = "org.scalameta"  %%   "scalameta"       % Versions.scalameta
  lazy val scalametaParadise     = "org.scalameta"  %    "paradise"        % Versions.scalametaParadise cross CrossVersion.full
}

object Dependencies {
  import Library._

  lazy val macros = List(
    scalaReflect,
    scalameta
  )

  lazy val root = List(
    shapeless,
    scalacheck
  )

}