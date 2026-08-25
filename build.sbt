//

// =====|  |=====

val Scala_3 = "3.7.4"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "slyce-zio"

ThisBuild / dynverVTagPrefix := false
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / version ~= (_.replace('+', '-'))
ThisBuild / dynver ~= (_.replace('+', '-'))

// The sbt-git bundled with sbt-ci-release 1.5.7 uses an old JGit that can't read a *linked git worktree*
// (it reports the repo as bare and throws NoWorkTreeException at load, blocking sbt from loading). That
// old plugin has no `useConsoleForROGit`, so we neutralize the JGit-backed read-only git settings to
// constants — JGit is then never invoked. Versioning is unaffected: it comes from sbt-dynver, which
// shells out to the git CLI (worktree-safe) independently of these keys.
ThisBuild / git.gitUncommittedChanges := false
ThisBuild / git.gitCurrentBranch := ""
ThisBuild / git.gitHeadCommit := None
ThisBuild / git.gitCurrentTags := Seq.empty[String]
ThisBuild / git.gitDescribedVersion := None
// These git.* keys are set to constants only to keep JGit from being invoked (see above); dynver owns
// versioning, so they're otherwise unused — exclude them from sbt's unused-key lint.
Global / excludeLintKeys ++= Set(
  git.gitUncommittedChanges,
  git.gitCurrentBranch,
  git.gitHeadCommit,
  git.gitCurrentTags,
  git.gitDescribedVersion,
)

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
        MyOrg %% "oxygen-core" % Versions.oxygen,
        MyOrg %% "oxygen-test" % Versions.oxygen % Test,
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      Test / fork := true,
    )

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

addCommandAlias("fmt", "scalafmtSbt; scalafmtAll;")
addCommandAlias("fmt-check", "scalafmtSbtCheck; scalafmtCheckAll;")
