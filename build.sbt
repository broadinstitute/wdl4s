import com.typesafe.sbt.GitPlugin.autoImport._
import sbt.Keys._

name := "wdl4s"

organization := "org.broadinstitute"

scalaVersion := "2.12.2"

crossScalaVersions := Seq("2.11.8", "2.12.2")

lazy val versionSettings = Seq(
  // Upcoming release, or current if we're on the master branch
  git.baseVersion := "0.14",

  // Shorten the git commit hash
  git.gitHeadCommit := git.gitHeadCommit.value map { _.take(7) },

  // Travis will deploy tagged releases, add -SNAPSHOT for all local builds
  git.gitUncommittedChanges := true,

  // For now, obfuscate SNAPSHOTs from sbt's developers: https://github.com/sbt/sbt/issues/2687#issuecomment-236586241
  git.uncommittedSignifier := Option("SNAP")
)

versionWithGit ++ versionSettings

val sprayJsonV = "1.3.2"
val circeVersion = "0.8.0"
val lenthallV = "0.25"

resolvers ++= List(
  "Broad Artifactory Releases" at "https://broadinstitute.jfrog.io/broadinstitute/libs-release/"
)

libraryDependencies ++= {
  Seq(
    "org.broadinstitute" %% "lenthall" % lenthallV,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    "io.spray" %% "spray-json" % sprayJsonV,
    /*
    Exclude test framework cats-laws and its transitive dependency scalacheck.
    If sbt detects scalacheck, it tries to run it.
    Explicitly excluding the two problematic artifacts instead of including the three (or four?).
    https://github.com/typelevel/cats/tree/v0.7.2#getting-started
     */
    "org.typelevel" %% "cats" % "0.9.0"
      exclude("org.typelevel", "cats-laws_2.11")
      exclude("org.typelevel", "cats-laws_2.12")
      exclude("org.typelevel", "cats-kernel-laws_2.11")
      exclude("org.typelevel", "cats-kernel-laws_2.12"),
    "commons-codec" % "commons-codec" % "1.10",
    "commons-io" % "commons-io" % "2.5",
    "org.apache.commons" % "commons-lang3" % "3.4",
    "com.github.pathikrit" %% "better-files" % "2.17.1",

    "io.circe" %% "circe-yaml" % "0.6.1",

    "eu.timepit" %% "refined"            % "0.8.2",
    "com.github.benhutchison" %% "mouse" % "0.8",

    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "com.lihaoyi" %% "ammonite-ops" % "1.0.0-RC7" % "test",
    "org.pegdown" % "pegdown" % "1.6.0" % Test
  )
}

libraryDependencies ++= Seq(
  "generic",
  "generic-extras",
  "shapes",
  "refined",
  "literal"
).map(m => "io.circe" %% s"circe-$m" % circeVersion)


// The reason why -Xmax-classfile-name is set is because this will fail
// to build on Docker otherwise.  The reason why it's 200 is because it
// fails if the value is too close to 256 (even 254 fails).  For more info:
//
// https://github.com/sbt/sbt-assembly/issues/69
// https://github.com/scala/pickling/issues/10
//
// Other fancy flags from https://tpolecat.github.io/2017/04/25/scalac-flags.html.
//
// Per JG's work in Cromwell, the following can't be turned on without causing piles of errors in wdl4s.  Many of the
// constructs that are flagged look suspicious and probably warrant further scrutiny, but no time for that now.
//
// "-Ywarn-unused:params"              // Warn if a value parameter is unused.

val baseOptions = List(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-explaintypes",
  "-Xmax-classfile-name", "200",
  "-target:jvm-1.8",
  "-encoding", "UTF-8"
)

val warningOptions = List(
  "-Xfuture",
  "-Xlint:adapted-args",
  "-Xlint:by-name-right-associative",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-override",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Xlint:unsound-match",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-inaccessible",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:privates",
  "-Ywarn-unused:locals",
  "-Ywarn-unused:patvars"
)

val consoleHostileOptions = List(
  "-Ywarn-unused:imports", // warns about every unused import on every command.
  "-Xfatal-warnings"       // makes those warnings fatal.
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, 12)) =>
    // The default scalacOptions includes console-hostile options.  These options are overridden specifically below
    // for the `console` target.
    baseOptions ++ warningOptions ++ consoleHostileOptions
  case Some((2, 11)) =>
    // Scala 2.11 takes a simplified set of options
    baseOptions
  case wut => throw new NotImplementedError(s"Found unsupported Scala version $wut. wdl4s does not support versions of Scala other than 2.11 or 2.12.")
})

// http://stackoverflow.com/questions/31488335/scaladoc-2-11-6-fails-on-throws-tag-with-unable-to-find-any-member-to-link#31497874
scalacOptions in(Compile, doc) := (baseOptions ++ List("-no-link-warnings"))
// No console-hostile options, otherwise console is effectively unusable.
// https://github.com/sbt/sbt/issues/1815
scalacOptions in(Compile, console) := (baseOptions ++ warningOptions)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDSI", "-h", "target/test-reports")
