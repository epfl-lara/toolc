name := "toolc"

mainClass in (Compile, run) := Some("toolc.Main")

mainClass in (Compile, packageBin) := Some("toolc.Main")

version := "3.0"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

proguardSettings

ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings")

ProguardKeys.proguardVersion in Proguard := "5.2.1"

ProguardKeys.inputFilter in Proguard := { file =>
  file.name match {
    case "manifest.jar"  => None
    case _               => Some("!META-INF/**")
  }
}

ProguardKeys.options in Proguard += ProguardOptions.keepMain("toolc.Main")

//def ghProject(repo: String, version: String) = RootProject(uri(s"${repo}#${version}"))
//val grammars = ghProject(
//  "https://github.com/epfl-lara/GrammarComparison.git",
//  "8f6963a8e3c9006ae5cac33a8c0a52d26c070856"
//)

//lazy val root = (project in file(".")).dependsOn(grammars)
