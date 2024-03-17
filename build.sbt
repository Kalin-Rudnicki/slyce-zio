//

// =====|  |=====

val Scala_3 = "3.3.0"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "slyce-zio"

ThisBuild / dynverVTagPrefix := false
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / watchBeforeCommand := Watch.clearScreen

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
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  ),
)

lazy val testAndCompile = "test->test;compile->compile"

// =====|  |=====

lazy val `slyce-core` =
  project
    .in(file("modules/slyce-core"))
    .settings(
      name := "slyce-core",
      libraryDependencies ++= Seq(
        MyOrg %% "harness-core" % Versions.harness,
        MyOrg %% "harness-zio-test" % Versions.harness % Test,
        "com.lihaoyi" %% "scalatags" % Versions.scalaTags,
        "com.github.julien-truffaut" %% "monocle-macro" % Versions.monocle,
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      Test / fork := true,
    )

lazy val `slyce-parse` =
  project
    .in(file("modules/slyce-parse"))
    .settings(
      name := "slyce-parse",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`slyce-core` % testAndCompile)

lazy val `slyce-parse-exe` =
  project
    .in(file("modules/slyce-parse-exe"))
    .settings(
      name := "slyce-parse-exe",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % Versions.harness,
      ),
    )
    .dependsOn(`slyce-parse` % testAndCompile)

lazy val `slyce-generate` =
  project
    .in(file("modules/slyce-generate"))
    .settings(
      name := "slyce-generate",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      // version := "2.1.3",
      assemblyJarName := s"../../../../jars/${name.value}-${version.value}.jar",
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % Versions.harness,
      ),
    )
    .dependsOn(`slyce-parse` % testAndCompile)

lazy val `slyce-test` =
  project
    .in(file("modules/slyce-test"))
    .settings(
      name := "slyce-test",
      publish / skip := true,
      Test / fork := true,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`slyce-parse-exe` % testAndCompile)

// TODO (KR) : IDEA-PLUGIN
/*
lazy val `slyce-idea-plugin` =
  project
    .in(file("modules/slyce-idea-plugin"))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      name := "slyce-idea-plugin",
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`slyce-generate` % testAndCompile)
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
