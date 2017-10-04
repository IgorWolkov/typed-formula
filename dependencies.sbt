
libraryDependencies ++= {
  val shapelessV  = "2.3.2"
  val scalacheckV = "1.13.4"

  Seq(
    "com.chuusai"     %% "shapeless"    % shapelessV,
    "org.scalacheck"  %% "scalacheck"   % scalacheckV    % "test"
  )
}