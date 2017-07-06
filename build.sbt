import com.typesafe.sbt.GitPlugin.autoImport._
import sbt.Keys._

name := "wdl4s"

organization := "org.broadinstitute"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")

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
      exclude("org.typelevel", "cats-kernel-laws_2.11"),
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
  "shapes",
  "refined"
).map(m => "io.circe" %% s"circe-$m" % circeVersion)


// The reason why -Xmax-classfile-name is set is because this will fail
// to build on Docker otherwise.  The reason why it's 200 is because it
// fails if the value is too close to 256 (even 254 fails).  For more info:
//
// https://github.com/sbt/sbt-assembly/issues/69
// https://github.com/scala/pickling/issues/10
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xmax-classfile-name", "200")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// http://stackoverflow.com/questions/31488335/scaladoc-2-11-6-fails-on-throws-tag-with-unable-to-find-any-member-to-link#31497874
scalacOptions in (Compile, doc) ++= Seq("-no-link-warnings")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDSI", "-h", "target/test-reports")
