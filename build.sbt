lazy val goose= (project in file(".")).settings(
  scalaVersion := "2.12.5",
  organization := "com.github.merlijn",
  libraryDependencies ++= Seq(
    "ch.qos.logback" %  "logback-classic" % "1.2.2",
    "org.typelevel"  %% "cats-effect" % "1.2.0"
  )
)

