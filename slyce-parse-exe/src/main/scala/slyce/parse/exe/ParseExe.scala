package slyce.parse.exe

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.cli.*
import harness.core.*
import harness.zio.*
import zio.*

import slyce.core.*
import slyce.parse.Parser as SlyceParser

object ParseExe {

  private final case class Config(files: NonEmptyList[String])
  private object Config {

    val parser: Parser[Config] =
      (
        Parser.values.nel[String](LongName.unsafe("file")),
      ).map(Config.apply)

  }

  private def fileRec(file: Path, supportedFileTypes: Set[String]): SHTask[List[Path]] =
    file.exists.flatMap {
      case true =>
        file.isFile.flatMap {
          case true =>
            if (file.pathName.ext.fold(false)(supportedFileTypes.contains)) ZIO.succeed(file :: Nil)
            else Logger.log.warning(s"Ignoring file, invalid extension: $file").as(Nil)
          case false =>
            file.isDirectory.flatMap {
              case true =>
                Logger.log.detailed(s"Searching in directory: $file") *>
                  file.children.flatMap { children => ZIO.foreach(children.toList)(fileRec(_, supportedFileTypes)) }.map(_.flatten)
              case false => Logger.log.warning(s"File is not file or directory? : $file").as(Nil)
            }
        }
      case false => Logger.log.warning(s"File does not exist: $file").as(Nil)
    }

  private def getFiles(config: Config, supportedFileTypes: Set[String]): SHTask[List[Path]] =
    ZIO.foreach(config.files.toList)(Path(_).flatMap(fileRec(_, supportedFileTypes))).map(_.flatten)

  private def lexExe(parser: SlyceParser, supportedFileTypes: Set[String]): Executable =
    Executable
      .withParser(Config.parser)
      .withEffect { config =>
        for {
          _ <- Logger.log.info("=====| Running Lex |=====")
          files <- getFiles(config, supportedFileTypes)
          _ <- ZIO.foreachDiscard(files) { file =>
            for {
              str <- file.readString
              source = Source(str, file.pathName.name.some)
              res = parser.lexer.tokenize(source)
              _ <- res match {
                case Right(toks)  => Logger.log.info(source.mark(toks.map(tok => Marked(tok.tokName, tok.span))))
                case Left(errors) => Logger.log.error(source.mark(errors.toList))
              }
            } yield ()
          }
        } yield ()
      }

  private def stressTestExe(parser: SlyceParser, supportedFileTypes: Set[String]): Executable =
    Executable
      .withParser(Config.parser)
      .withEffect { config =>
        for {
          _ <- Logger.log.info("=====| Running Stress Test |=====")
          files <- getFiles(config, supportedFileTypes)
          _ <- ZIO.foreachDiscard(files) { file =>
            for {
              str <- file.readString
              source = Source(str, file.pathName.name.some)
              _ <- ZIO.succeed(parser.lexer.tokenize(source)).timed.flatMap {
                case (duration1, Right(toks)) =>
                  ZIO.succeed(parser.grammar.buildTree(source, toks)).timed.flatMap {
                    case (duration2, Right(_)) =>
                      Logger.log
                        .info(s"$file\n  - duration.lexer: ${duration1.prettyPrint}\n  - duration.grammar: ${duration2.prettyPrint}\n  - duration.total: ${(duration1 + duration2).prettyPrint}")
                    case (duration2, Left(errors)) =>
                      Logger.log.error(
                        s"$file\n  - duration.lexer: ${duration1.prettyPrint}\n  - duration.grammar: ${duration2.prettyPrint}\n  - duration.total: ${(duration1 + duration2).prettyPrint}\n${source
                          .mark(errors.toList)}",
                      )
                  }
                case (duration1, Left(errors)) =>
                  Logger.log.error(s"$file\n  - duration.lexer: ${duration1.prettyPrint}\n${source.mark(errors.toList)}")
              }
            } yield ()
          }
        } yield ()
      }

  def fromParser(parer: SlyceParser)(supportedFileType0: String, supportedFileTypeN: String*): Executable = {
    val supportedFileTypes: Set[String] = supportedFileTypeN.toSet + supportedFileType0

    Executable.fromSubCommands(
      "lex" -> lexExe(parer, supportedFileTypes),
      "stress-test" -> stressTestExe(parer, supportedFileTypes),
    )
  }

}
