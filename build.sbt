scalaVersion := "2.12.4"

organization := "ch.becompany"

name := "tagless-final-example"

libraryDependencies ++= Seq(
  "org.atnos" %% "eff" % "4.5.0",
  "com.chuusai" %% "shapeless" % "2.3.3"
)

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred for scala 2.12.x
scalacOptions += "-Ypartial-unification"
