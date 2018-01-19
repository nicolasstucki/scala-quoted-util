val dottyVersion = "0.6.0-RC1"

lazy val root = (project in file(".")).
  settings(
    name := "quote-util",
    version := "0.0.1",

    scalaVersion := dottyVersion,

    testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.6" % "0.6.0-RC1" % "test->runtime",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
