import sbt._
import Keys._

object BuildSettings
{
  val buildOrganization = "edu.berkeley.cs"
  val buildVersion = "1.1"
  val buildScalaVersion = "2.9.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )

}

object ChiselBuild extends Build
{
  import BuildSettings._

  lazy val chisel = Project("chisel", file("chisel"), settings = buildSettings)
  lazy val hardfloat = Project("hardfloat", file("hardfloat"), settings = buildSettings) dependsOn(chisel)
  lazy val hwacha = Project("hwacha", file("hwacha"), settings = buildSettings) dependsOn(chisel, hardfloat)
}
