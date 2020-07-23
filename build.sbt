name           := "games"
scalaVersion   := "2.13.0"
scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")
mainClass in Compile := Some("main.Main")
scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "1.0.0",
)
enablePlugins(ScalaJSPlugin)
