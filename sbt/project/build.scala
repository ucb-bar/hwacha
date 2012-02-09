import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "edu.berkeley.cs"
  val buildVersion = "1.1"
  val buildScalaVersion = "2.8.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )

}

object ChiselBuild extends Build{
  import BuildSettings._

  lazy val myvec = Project("vector", file("."), settings = buildSettings) aggregate(vector)
  lazy val vector = Project("hwacha", file("hwacha"), settings = buildSettings)
}
