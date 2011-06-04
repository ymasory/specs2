/** Project */
name := "specs2"
version := "1.1-SNAPSHOT"
organization := "org.specs2"
scalaVersion := "2.8.1"

/** Shell */
shellPrompt := { state => System.getProperty("user.name") + "> " }
shellPrompt in ThisBuild := { state => Project.extract(state).cid + "> " }

/** Dependencies */
resolvers ++= Seq("snapshots-repo" at "http://scala-tools.org/repo-snapshots", 
                  "Local Maven Repository" at "file://$M2_REPO")

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.8", 
  "org.scala-tools.testing" % "test-interface" % "0.5", 
  "com.googlecode.scalaz" %% "scalaz-core" % "5.1-SNAPSHOT",
  "org.hamcrest" % "hamcrest-all" % "1.1",
  "org.mockito" % "mockito-all" % "1.8.5",
  "junit" % "junit" % "4.7",
  "org.pegdown" % "pegdown" % "0.9.1"
)

/** Compilation */
javacOptions ++= Seq("-Xmx256m", "-Xms64m", "-Xss4m")
scalacOptions += "-deprecation"
maxErrors := 20
pollInterval := 1000

/** Console */
initialCommands in console := "import org.specs2._"

/** Packaging */

/** Publishing */

Credentials(Path.userHome / ".ivy2" / ".credentials", log)

val nexusDirect = "http://nexus-direct.scala-tools.org/content/repositories/"
publishTo := if (version.toString.endsWith("SNAPSHOT")) Some("snapshots" at nexusDirect + "snapshots/")
	           else                                       Some("releases" at nexusDirect + "releases/")
