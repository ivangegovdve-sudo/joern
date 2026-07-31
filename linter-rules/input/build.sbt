name := "linter-rules-input"
scalaVersion := "2.13.18"
disablePlugins(ScalafixPlugin)
scalacOptions := Seq("-deprecation", "-feature", "-Yrangepos")
// testkit input fixtures only — never released
publish / skip := true
