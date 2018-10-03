resolvers += "ruimo.com" at "http://static.ruimo.com/release"

name := "FormBuilderCommon"
fork in run := true
scalacOptions := Seq("-unchecked", "-deprecation", "-feature")
libraryDependencies ++= Seq(
  "com.ruimo" %% "scoins" % "1.21",
  "com.ruimo" %% "graphics" % "1.12",
  "com.typesafe.play" % "play-json_2.12" % "2.6.8",
  "org.specs2" %% "specs2-core" % "4.0.0" % "test"
)

lazy val root = (project in file(".")).
  enablePlugins(JavaAppPackaging, BuildInfoPlugin).
  settings(
    organization := "com.ruimo",
    scalaVersion := "2.12.4",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "generated"
  )

publishTo := Some(
  Resolver.file(
    "formbuildercommon",
    new File(Option(System.getenv("RELEASE_DIR")).getOrElse("/tmp"))
  )
)
