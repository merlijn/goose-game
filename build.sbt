lazy val goose= (project in file(".")).settings(
  scalaVersion := "2.12.5",
  organization := "com.github.merlijn",
  libraryDependencies ++= Seq(
    "org.typelevel"  %% "cats-effect" % "1.2.0"
  )
)

