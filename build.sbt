val dottyVersion = "3.1.0"
val dottyCIVersions = Seq("3.1.0")

lazy val root = (project in file(".")).
  settings(
    name := "quoted-util",
    version := "0.0.1",
    organization := "io.github.nicolasstucki",
    scalaVersion := dottyVersion,

    Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % dottyVersion % "test",
      "com.novocode" % "junit-interface" % "0.11" % "test",
    ),

    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    pomExtra :=
      <url>https://github.com/nicolasstucki/quoted-util</url>
      <licenses>
        <license>
          <name>BSD-style</name>
          <url>http://www.opensource.org/licenses/bsd-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:nicolasstucki/quoted-util.git</url>
        <connection>scm:git:git@github.com:nicolasstucki/quoted-util.git</connection>
      </scm>
      <developers>
        <developer>
          <id>nicolas.stucki</id>
          <name>Nicolas Stucki</name>
          <url>http://io.github.nicolasstucki</url>
        </developer>
      </developers>
  )

ThisBuild / githubWorkflowJavaVersions := Seq("8", "15")
ThisBuild / githubWorkflowScalaVersions := dottyCIVersions
ThisBuild / githubWorkflowBuildPostamble := Seq(
  WorkflowStep.Run(List("sbt test")),
)
ThisBuild / githubWorkflowPublishTargetBranches := Nil
Global / onChangedBuildSource := ReloadOnSourceChanges
