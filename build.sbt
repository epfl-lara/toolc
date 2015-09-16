name := "toolc"

mainClass in (Compile, run) := Some("toolc.Main")

mainClass in (Compile, packageBin) := Some("toolc.Main")

version := "2.7"

scalaVersion := "2.11.7"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

proguardSettings

ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings")

ProguardKeys.inputFilter in Proguard := { file =>
  file.name match {
    case "manifest.jar"  => None
    case _               => Some("!META-INF/**")
  }
}

ProguardKeys.options in Proguard += ProguardOptions.keepMain("toolc.Main")
