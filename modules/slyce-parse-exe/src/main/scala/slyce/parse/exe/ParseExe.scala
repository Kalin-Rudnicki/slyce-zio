package slyce.parse.exe

import oxygen.predef.core.*
import oxygen.predef.executable.{*, given}
import oxygen.predef.zio.*
import zio.*

import slyce.core.*
import slyce.parse.Parser as SlyceParser

object ParseExe {

  private final case class Config(files: NonEmptyList[String])
  private object Config {

    val parser: Params[Config] =
      (
        Params.value[String]("file").repeatedNel
      ).map(Config.apply)

  }

  private def fileRec(file: Path, supportedFileTypes: Set[String]): Task[List[Path]] =
    file.exists.flatMap {
      case true =>
        file.`type`.flatMap {
          case Path.Type.File =>
            if file.fileName.`extension`.fold(false)(supportedFileTypes.contains) then ZIO.succeed(file :: Nil)
            else ZIO.logWarning(s"Ignoring file, invalid extension: $file").as(Nil)
          case Path.Type.Directory =>
            ZIO.logDetailed(s"Searching in directory: $file") *>
              file.children.flatMap { children => ZIO.foreach(children.toList)(fileRec(_, supportedFileTypes)) }.map(_.flatten)
          case Path.Type.Other =>
            ZIO.logWarning(s"File is not file or directory? : $file").as(Nil)
        }
      case false =>
        ZIO.logWarning(s"File does not exist: $file").as(Nil)
    }

  private def getFiles(config: Config, supportedFileTypes: Set[String]): Task[List[Path]] =
    ZIO.foreach(config.files.toList)(Path.of(_).flatMap(fileRec(_, supportedFileTypes))).map(_.flatten)

  private def lexExe(parser: SlyceParser, supportedFileTypes: Set[String]): Executable =
    Executable
      .withCLIParser(Config.parser)
      .withExecute { config =>
        for {
          _ <- ZIO.logInfo("=====| Running Lex |=====")
          files <- getFiles(config, supportedFileTypes)
          _ <- ZIO.foreachDiscard(files) { file =>
            for {
              str <- file.read
              source = Source(str, file.pathName.name.some)
              res = parser.lexer.tokenize(source)
              _ <- res match {
                case Right(toks)  => ZIO.logInfo(source.mark(toks.map(tok => Marked(tok.tokName, tok.span))))
                case Left(errors) => ZIO.logError(source.mark(errors.toList))
              }
            } yield ()
          }
        } yield ()
      }

  private def stressTestExe(parser: SlyceParser, supportedFileTypes: Set[String]): Executable =
    Executable
      .withCLIParser(Config.parser)
      .withExecute { config =>
        for {
          _ <- ZIO.logInfo("=====| Running Stress Test |=====")
          files <- getFiles(config, supportedFileTypes)
          _ <- ZIO.foreachDiscard(files) { file =>
            for {
              str <- file.read
              source = Source(str, file.pathName.name.some)
              _ <- ZIO.succeed(parser.lexer.tokenize(source)).timed.flatMap {
                case (duration1, Right(toks)) =>
                  ZIO.succeed(parser.grammar.buildTree(source, toks)).timed.flatMap {
                    case (duration2, Right(_)) =>
                      ZIO.logInfo(s"$file\n  - duration.lexer: ${duration1.render}\n  - duration.grammar: ${duration2.render}\n  - duration.total: ${(duration1 + duration2).render}")
                    case (duration2, Left(errors)) =>
                      ZIO.logError(
                        s"$file\n  - duration.lexer: ${duration1.render}\n  - duration.grammar: ${duration2.render}\n  - duration.total: ${(duration1 + duration2).render}\n${source
                            .mark(errors.toList)}",
                      )
                  }
                case (duration1, Left(errors)) =>
                  ZIO.logError(s"$file\n  - duration.lexer: ${duration1.render}\n${source.mark(errors.toList)}")
              }
            } yield ()
          }
        } yield ()
      }

  def fromParser(parer: SlyceParser)(supportedFileType0: String, supportedFileTypeN: String*): Executable = {
    val supportedFileTypes: Set[String] = supportedFileTypeN.toSet + supportedFileType0

    Executable.oneOf(
      "lex" -> lexExe(parer, supportedFileTypes),
      "stress-test" -> stressTestExe(parer, supportedFileTypes),
    )
  }

}
