name := "linter-rules"
//scalafix otherwise gets unhappy because of circular dependency
disablePlugins(ScalafixPlugin)

libraryDependencies += "ch.epfl.scala" % "scalafix-core_2.13" % _root_.scalafix.sbt.BuildInfo.scalafixVersion

libraryDependencies += "ch.epfl.scala" % "scalafix-testkit_2.13.18" % _root_.scalafix.sbt.BuildInfo.scalafixVersion % Test

Test / resourceGenerators += Def.task {
  val inputClasspath =
    (LocalProject("linterRulesInput") / Compile / fullClasspath).value
  val inputSourceDirs =
    (LocalProject("linterRulesInput") / Compile / unmanagedSourceDirectories).value
  val sourceroot = (ThisBuild / baseDirectory).value
  val scalaVer =
    (LocalProject("linterRulesInput") / scalaVersion).value
  val scalacOpts =
    (LocalProject("linterRulesInput") / Compile / scalacOptions).value

  val props = new java.util.Properties()
  def putFiles(key: String, files: Seq[java.io.File]): Unit = {
    props.put(
      key,
      files.iterator.filter(_.exists()).mkString(java.io.File.pathSeparator)
    )
  }

  putFiles("inputClasspath", inputClasspath.map(_.data))
  putFiles("inputSourceDirectories", inputSourceDirs)
  putFiles("outputSourceDirectories", Seq.empty[java.io.File])
  putFiles("sourceroot", Seq(sourceroot))
  props.put("scalaVersion", scalaVer)
  props.put("scalacOptions", scalacOpts.mkString("|"))

  val outputFile =
    (Test / managedResourceDirectories).value.head / "scalafix-testkit.properties"
  IO.write(props, "Input data for scalafix testkit", outputFile)
  Seq(outputFile)
}
