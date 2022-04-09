//

// =====|  |=====

val Scala_3 = "3.1.2-RC3"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "slyce-fp"

ThisBuild / dynverVTagPrefix := false
ThisBuild / dynverSonatypeSnapshots := true

ThisBuild / version ~= (_.replace('+', '-'))
ThisBuild / dynver ~= (_.replace('+', '-'))

// =====|  |=====

inThisBuild(
  Seq(
    organization := MyOrg,
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("public"),
    ),
    //
    description := "A (flex/bison)-esque parser generator for scala.",
    licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
    homepage := Some(url(s"https://github.com/$githubUsername/$githubProject")),
    developers := List(
      Developer(
        id = "Kalin-Rudnicki",
        name = "Kalin Rudnicki",
        email = "kalin.rudnicki@gmail.com",
        url = url(s"https://github.com/$githubUsername"),
      ),
    ),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    scalaVersion := Scala_3,
    scalacOptions += "-source:future",
  ),
)

// =====|  |=====

lazy val `slyce-core` =
  project
    .in(file("slyce-core"))
    .settings(
      name := "slyce-core",
      libraryDependencies ++= Seq(
        MyOrg %% "klib" % "2.0.1",
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )

lazy val `slyce-generate` =
  project
    .in(file("slyce-generate"))
    .settings(
      name := "slyce-generate",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`slyce-core`)

lazy val `slyce-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .aggregate(
      `slyce-core`,
    )
