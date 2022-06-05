package slyce.generate

import cats.syntax.either.*
import cats.syntax.option.*
import klib.utils.*
import klib.utils.commandLine.parse.*
import zio.*

import slyce.core.*
import slyce.generate.builder.Builders.*
import slyce.generate.debugging.Result as DebugResult
import slyce.generate.grammar.*
import slyce.generate.lexer.*
import slyce.generate.output.*

object TestMain extends ExecutableApp {

  private def makeExe(
      name: String,
      lexerInput: LexerInput,
      grammarInput: GrammarInput,
  ): (String, Executable) = {
    (
      name,
      Executable
        .fromParser(Parser.unit.disallowExtras)
        .withLayer { _ => ZIO.unit.toLayer }
        .withExecute { _ =>
          for {
            _ <- Logger.println.info(s"=====| TestMain : $name |=====")

            debugResult = DebugResult.build(lexerInput, grammarInput)
            debugResultFrag = DebugResult.resultToHTML(debugResult)
            debugResultString = debugResultFrag.render

            debugOutputFile <- File.fromPath("target/test-output.html")
            _ <- debugOutputFile.writeString(debugResultString)

            result <- ZIO.fromEither(Result.build(lexerInput, grammarInput).leftMap(_.map(e => KError.UserError(e.toString))))
            resultString = formatters.Scala3.format(List("a", "b", "c"), "Tmp", result)
            _ <- Logger.println.info(resultString)
            _ <- Logger.println.info(s"Generated ${resultString.count(_ == '\n')} line(s)")
          } yield ()
        },
    )
  }

  private val calc: (String, Executable) =
    makeExe(
      "calc",
      lexerInput = lexer("General")(
        lexer.mode("General")(
          lexer.mode.line(Regex.CharClass.inclusive('+', '-'))(Yields.Yield.Terminal("addOp")),
          lexer.mode.line(Regex.CharClass.inclusive('*', '/'))(Yields.Yield.Terminal("multOp")),
          lexer.mode.line(Regex.CharClass.inclusive('^'))(Yields.Yield.Terminal("powOp")),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('-').optional,
              Regex.CharClass.`\\d`.atLeastOnce,
            ),
          )(Yields.Yield.Terminal("int")),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('-').optional,
              Regex.CharClass.`\\d`.atLeastOnce,
              Regex.CharClass.inclusive('.'),
              Regex.CharClass.`\\d`.atLeastOnce,
            ),
          )(Yields.Yield.Terminal("float")),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[a-z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("variable")),
          lexer.mode.line(Regex.CharClass.inclusive('=', '(', ')', ';', '~', ','))(Yields.Yield.Text()),
          lexer.mode.line(Regex.Sequence("->"))(Yields.Yield.Text()),
          lexer.mode.line(Regex.CharClass.inclusive(' ', '\t', '\n'))(),
        ),
      ),
      grammarInput = grammar("Lines", maxLookAhead = 2)(
        grammar.nt.+(
          grammar.liftElements()("Line")(";"),
        )("Lines"),
        grammar.nt.`:`(
          grammar.elements("Assign"),
          grammar.elements("Expr", "~".optional),
        )("Line"),
        grammar.nt.`:`(
          grammar.elements("variable", "=", "Expr"),
          grammar.elements("variable", "=", "FunctionDef"),
        )("Assign"),
        grammar.nt.^(
          grammar.liftElements("[")("Lines")("]"),
          grammar.liftElements("[", "-")("Expr")("]"),
        )("Tmp"),
        grammar.nt.~(
          "powOp".asRight,
          "multOp".asLeft,
          "addOp".asLeft,
        )(
          grammar.nt.^(
            grammar.liftElements()("variable")(),
            grammar.liftElements()("int")(),
            grammar.liftElements()("float")(),
            grammar.liftElements("(")("Expr")(")"),
          ),
        )("Expr"),
        grammar.nt.`:`(
          grammar.elements(
            "(",
            grammar.nt.*(
              grammar.liftElements()("variable")(),
              grammar.liftElements(",")("variable")(),
            ),
            ")",
            "->",
            "Expr",
          ),
        )("FunctionDef"),
        grammar.nt.`:`(
          grammar.elements(
            "variable",
            "(",
            grammar.nt.*(
              grammar.liftElements()("Expr")(),
              grammar.liftElements(",")("Expr")(),
            ),
            // ",".optional,
            ")",
          ),
        )("FunctionCall"),
      ),
    )

  private val simpleExpr: (String, Executable) =
    makeExe(
      "simple-expr",
      lexerInput = lexer("General")(
        lexer.mode("General")(
          lexer.mode.line(Regex.CharClass.inclusive('+', '-'))(Yields.Yield.Text()),
          lexer.mode.line(Regex.CharClass.inclusive('*', '/'))(Yields.Yield.Text()),
          lexer.mode.line(Regex.CharClass.inclusive('^'))(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('-').optional,
              Regex.CharClass.`\\d`.atLeastOnce,
            ),
          )(Yields.Yield.Terminal("int")),
          lexer.mode.line(Regex.CharClass.inclusive('(', ')'))(Yields.Yield.Text()),
          lexer.mode.line(Regex.CharClass.inclusive(' ', '\t', '\n'))(),
        ),
      ),
      grammarInput = grammar("Expr")(
        grammar.nt.~(
          "PowOp".asRight,
          "MultOp".asLeft,
          "AddOp".asLeft,
        )(
          grammar.nt.^(
            grammar.liftElements()("int")(),
            grammar.liftElements("(")("Expr")(")"),
          ),
        )("Expr"),
        grammar.nt.`:`(
          grammar.elements("+"),
          grammar.elements("-"),
        )("AddOp"),
        grammar.nt.`:`(
          grammar.elements("*"),
          grammar.elements("/"),
        )("MultOp"),
        grammar.nt.`:`(
          grammar.elements("^"),
        )("PowOp"),
      ),
    )

  private val causeConflict: (String, Executable) =
    makeExe(
      "cause-conflict",
      lexerInput = lexer("General")(
        lexer.mode("General")(
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[a-z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("variable")),
          lexer.mode.line(Regex.CharClass.inclusive('{', '}', ':', ','))(Yields.Yield.Text()),
        ),
      ),
      grammarInput = grammar("Base")(
        grammar.nt.`:`(
          grammar.elements(
            "{",
            grammar.nt.*(
              grammar.liftElements()("variable")(),
              grammar.liftElements(",")("variable")(),
            ),
            "}",
          ),
          grammar.elements(
            "{",
            grammar.nt.*(
              grammar.liftElements()("NamedArg")(),
              grammar.liftElements(",")("NamedArg")(),
            ),
            "}",
          ),
        )("Base"),
        grammar.nt.`:`(
          grammar.elements("variable", ":", "variable"),
        )("NamedArg"),
      ),
    )

  override val executable: Executable =
    Executable.fromSubCommands(
      calc,
      simpleExpr,
      causeConflict,
    )

}
