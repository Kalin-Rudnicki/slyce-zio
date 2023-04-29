package slyce.generate

import harness.core.{given, *}
import harness.core.commandLine.parse.*
import zio.*

import slyce.core.*
import slyce.parse.Parser as SlyceParser

object StressTest extends ExecutableApp {

  private final case class Case(
      name: String,
      parsers: List[(String, SlyceParser)],
      paths: List[String],
  )

  private val cases: List[Case] =
    List(
      Case(
        "sgf",
        List(
          "current" -> slyce.generate.parsers.Grammar,
          // "snapshot" -> slyce.generate.parsers.GrammarSnapshot,
        ),
        List(
          "/home/kalin/dev/current/slyce-zio/slyce-generate/src/main/slyce/slyce/generate/parsers/Grammar.sgf",
          "/home/kalin/dev/current/slyce-zio/stress-test/grammar/GrammarTmp1.sgf",
          "/home/kalin/dev/current/slyce-zio/stress-test/grammar/GrammarTmp2.sgf",
          "/home/kalin/dev/current/slyce-zio/stress-test/grammar/GrammarTmp3.sgf",
        ),
      ),
    )

  private def execCase(repeat: Int, _case: Case): SKTask[Unit] =
    Logger.println.info(_case.name) *>
      Logger.withIndent(1) {
        ZIO.traverseNEL(_case.paths)(execPath(repeat, _case, _)).unit
      }

  private def execPath(repeat: Int, _case: Case, path: String): SKTask[Unit] =
    Logger.println.info(path) *>
      Logger.withIndent(1) {
        for {
          file <- File.fromPath(path)
          size <- file.size
          source <- Source.fromFile(file)
          _ <- Logger.println.info(s"${BigDecimal(size) * 100 / Int.MaxValue}% : ${(source.arr.count(_ == '\n') + 2).toStringCommas} lines")
          _ <- ZIO.traverseNEL(_case.parsers)(execParser(repeat, source, _, _))
        } yield ()
      }

  private def execParser(repeat: Int, source: Source, name: String, parser: SlyceParser): SKTask[Unit] =
    Logger.println.info(name) *>
      Logger.withIndent(1) {
        for {
          results: List[(Long, Long)] <- exec(source, parser).replicateZIO(repeat).map(_.toList)
          avgTokenize = results.map(_._1).sum / results.length
          avgBuildTree = results.map(_._2).sum / results.length
          _ <- Logger.println.info(s"avg: (${avgTokenize.toStringCommas}, ${avgBuildTree.toStringCommas}) = ${(avgTokenize + avgBuildTree).toStringCommas}")
        } yield ()
      }

  private def exec(source: Source, parser: SlyceParser): SKTask[(Long, Long)] =
    for {
      (t, tokens) <- ZIO.succeed(parser.lexer.tokenize(source)).timed
      (bt, _) <- ZIO.succeed(parser.grammar.buildTree(source, tokens.toOption.get)).timed
      tokenize: Long = t.toMillis
      buildTree: Long = bt.toMillis
      _ <- Logger.println.info(s"(${tokenize.toStringCommas}, ${buildTree.toStringCommas})")
    } yield (tokenize, buildTree)

  override val executable: Executable =
    Executable
      .fromParser(Parser.singleValue[Int]("repeat").default(5).disallowExtras)
      .withLayer { _ => ZIO.unit.toLayer }
      .withExecute { repeat =>
        ZIO.traverseNEL(cases)(execCase(repeat.max(1), _)).unit
      }

}
