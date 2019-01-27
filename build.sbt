name := "lineout-simulator"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"
libraryDependencies += "org.typelevel" %% "cats-macros" % "1.5.0"
libraryDependencies += "org.typelevel" %% "cats-kernel" % "1.5.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"