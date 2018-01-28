val dottyVersion = dottyLatestNightlyBuild.get

lazy val root = (project in file(".")).
  settings(
    name := "quote-util",
    version := "0.0.1",

    scalaVersion := dottyVersion,

    testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.7" % dottyVersion % "test->runtime",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
