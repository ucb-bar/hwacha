import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "edu.berkeley.cs"
  val buildVersion = "1.1"
  val buildScalaVersion = "2.8.1"

  val radlabRepo = "Radlab Repository" at "http://scads.knowsql.org/nexus/content/groups/public/"
  // val radlabRepo = "releases" at "http://scads.knowsql.org/nexus/content/repositories/releases/"
  // val chisel     = "edu.berkeley.cs" %% "hcl" % "1.0"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    // unmanagedBase <<= baseDirectory { base => base / "custom_lib" },

    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion

    // resolvers    := Seq(radlabRepo)
  )

}

object ChiselBuild extends Build{
  import BuildSettings._

  lazy val mymem = Project("vector", file("."), settings = buildSettings) aggregate(memory)
  lazy val memory = Project("riscv-vector", file("riscv-vector"),
			    settings = buildSettings ++ Seq(libraryDependencies := chiselDeps))

  def chiselDeps = Seq(hcl)
  val hcl     = "edu.berkeley.cs" %% "hcl" % "1.1"
  // val hclftp  = "edu.berkeley.cs" % "hcl" % "1.0" from "http://www.cs.berkeley.edu/lib/slinky.jar";
}
