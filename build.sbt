//

// =====|  |=====

val Scala_3 = "3.1.3-RC5"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "slyce-zio"

ThisBuild / dynverVTagPrefix := false
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / version ~= (_.replace('+', '-'))
ThisBuild / dynver ~= (_.replace('+', '-'))

// =====|  |=====

lazy val HarnessVersion = "bad9f4a7bb92e62d9be5e1ba31731b129aa1cd8f"

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
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  ),
)

// =====|  |=====

lazy val `slyce-core` =
  project
    .in(file("slyce-core"))
    .settings(
      name := "slyce-core",
      libraryDependencies ++= Seq(
        MyOrg %% "harness-core" % HarnessVersion,
        MyOrg %% "harness-test" % HarnessVersion % Test,
        "com.lihaoyi" %% "scalatags" % "0.11.1",
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )

lazy val `slyce-parse` =
  project
    .in(file("slyce-parse"))
    .settings(
      name := "slyce-parse",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`slyce-core` % "test->test;compile->compile")

lazy val `slyce-parse-exe` =
  project
    .in(file("slyce-parse-exe"))
    .settings(
      name := "slyce-parse-exe",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % HarnessVersion,
      ),
    )
    .dependsOn(`slyce-parse` % "test->test;compile->compile")

lazy val `slyce-generate` =
  project
    .in(file("slyce-generate"))
    .settings(
      name := "slyce-generate",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      // version := "1.2.0",
      assemblyJarName := s"${name.value}-${version.value}.jar",
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % HarnessVersion,
      ),
    )
    .dependsOn(`slyce-parse` % "test->test;compile->compile")

lazy val `slyce-test` =
  project
    .in(file("slyce-test"))
    .settings(
      name := "slyce-test",
      publish / skip := true,
      Test / fork := true,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`slyce-parse-exe` % "test->test;compile->compile")

// TODO (KR) : IDEA-PLUGIN
/*
lazy val `slyce-idea-plugin` =
  project
    .in(file("slyce-idea-plugin"))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      name := "slyce-idea-plugin",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`slyce-generate` % "test->test;compile->compile")
 */

lazy val `slyce-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .aggregate(
      `slyce-core`,
      `slyce-generate`,
      `slyce-parse`,
      `slyce-parse-exe`,
      `slyce-test`,
      // TODO (KR) : IDEA-PLUGIN
      // `slyce-idea-plugin`
    )
