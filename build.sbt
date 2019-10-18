
// Building both for JVM and JavaScript runtimes.

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalaVer = "2.13.1"

val crossScalaVer = Seq(scalaVer)

lazy val commonSettings = Seq(
  name         := "formula",
  description  := "Lightweight XBRL formula support",
  organization := "eu.cdevreeze.xbrl.formula",
  version      := "0.3.0-SNAPSHOT",

  scalaVersion       := scalaVer,
  crossScalaVersions := crossScalaVer,

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Xlint", "-target:jvm-1.8"),

  publishArtifact in Test := false,
  publishMavenStyle := true,

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

  pomExtra := pomData,
  pomIncludeRepository := { _ => false },

  libraryDependencies += "eu.cdevreeze.yaidom" %%% "yaidom" % "1.10.1",

  libraryDependencies += "eu.cdevreeze.tqa" %%% "tqa" % "0.8.11",

  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % "test"
)

lazy val root = project.in(file("."))
  .aggregate(formulaJVM, formulaJS)
  .settings(commonSettings: _*)
  .settings(
    name                 := "formula",
    // Thanks, scala-java-time, for showing us how to prevent any publishing of root level artifacts:
    // No, SBT, we don't want any artifacts for root. No, not even an empty jar.
    publish              := {},
    publishLocal         := {},
    publishArtifact      := false,
    Keys.`package`       := file(""))

lazy val formula = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(commonSettings: _*)
  .jvmSettings(
    // This is the HE release of Saxon. You may want to use the EE release instead.

    libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "9.9.1-5"
  )
  .jsSettings(
    // Do we need this jsEnv?
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),

    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7",

    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC3",

    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.7.0" % "optional",

    parallelExecution in Test := false
  )

lazy val formulaJVM = formula.jvm
lazy val formulaJS = formula.js

lazy val pomData =
  <url>https://github.com/dvreeze/formula</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      <comments>Formula is licensed under Apache License, Version 2.0</comments>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:git@github.com:dvreeze/formula.git</connection>
    <url>https://github.com/dvreeze/formula.git</url>
    <developerConnection>scm:git:git@github.com:dvreeze/formula.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>dvreeze</id>
      <name>Chris de Vreeze</name>
      <email>chris.de.vreeze@caiway.net</email>
    </developer>
  </developers>

