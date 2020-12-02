# Hwacha Vector-Thread Co-Processor Sources

To use this co-processor, include this repo as a git submodule and add it as
to your chip's ``build.sbt`` as a Project, e.g.

```
lazy val hwacha = Project(file("hwacha"), "hwacha")
  .settings(buildSettings)
  .dependsOn(rocketchip)
```

Hwacha depends on the Rocket Chip project. Make sure the proper JARs are installed.
For more information on how to use this co-processor, refer to (https://github.com/ucb-bar/chipyard).
