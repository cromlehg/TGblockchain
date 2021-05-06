name := """techgen"""

version := "1.0-SNAPSHOT"


scalaVersion := "2.12.8"


resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")

val scorexVersion = "4bc8c385-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  ("org.scorexfoundation" %% "scorex-core" % scorexVersion).exclude("ch.qos.logback", "logback-classic"),
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scorexfoundation" %% "scorex-testkit" % scorexVersion % "test",
  "org.scalactic" %% "scalactic" % "3.0.5" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.msgpack" %% "msgpack-scala" % "0.8.13",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  
  "com.typesafe.play" %% "play-json" % "2.7.1",
  "com.typesafe.play" %% "play-ahc-ws-standalone" % "2.0.1",
  "com.typesafe.play" %% "play-ws-standalone-json" % "2.0.1"
)

exportJars := true

mainClass in (Compile, packageBin) := Some("techgen.TechGen")

enablePlugins(JavaAppPackaging)

rpmRelease := "1"

rpmVendor := "com.blockwit"

rpmUrl := Some("http://github.com/cromlehg/TechGen")

rpmLicense := Some("BSD")
