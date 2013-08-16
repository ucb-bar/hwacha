organization := "edu.berkeley.cs"

version := "1.2"

name := "hwacha"

scalaVersion := "2.10.2"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel" % "2.0",
                            "edu.berkeley.cs" %% "hardfloat" % "1.2")
