lazy val root = project.in(file("."))
  .settings(
    name := "dru-w4-lab1",
    version := "0.1",
    scalaVersion := "2.12.6",

    wartremoverErrors ++= Warts.unsafe ++ Seq(Wart.Recursion, Wart.While),
    wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "SimpleSpec.scala",
    wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "Lab1Spec.scala",

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.5",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test"
    )
  )

addCommandAlias("testLight", "testOnly SimpleSpec")
