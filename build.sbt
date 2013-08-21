organization := "edu.berkeley.cs"

version := "1.2"

name := "hwacha"

scalaVersion := "2.10.2"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
  "scct-github-repository" at "http://mtkopone.github.com/scct/maven-repo"
)

libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel" % "2.1-SNAPSHOT",
                            "edu.berkeley.cs" %% "hardfloat" % "1.2")
