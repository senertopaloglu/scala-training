lazy val root = Project(id = "functional-patterns", base = file("."))
  .settings(moduleName := "functional-patterns")
  .settings(
    publish         := {},
    publishLocal    := {},
    publishArtifact := false
  )
  .aggregate(json, codecs, parsecs, gen)

lazy val json    = project
lazy val codecs  = project.dependsOn(json)
lazy val parsecs = project.dependsOn(json)
lazy val gen     = project

logLevel := Level.Error
