package slyce.generate

import cats.syntax.either.*
import cats.syntax.option.*
import klib.utils.*
import klib.utils.commandLine.parse.*
import zio.*

import slyce.generate.builder.Builders.*
import slyce.generate.debugging.Result
import slyce.generate.grammar.*
import slyce.generate.lexer.*

object TestMain extends ExecutableApp {

  private def makeExe(
      name: String,
      lexerInput: LexerInput,
      grammarInput: GrammarInput,
  ): (String, Executable) =
    (
      name,
      Executable
        .fromParser(Parser.unit.disallowExtras)
        .withLayer { _ => ZIO.unit.toLayer }
        .withExecute { _ =>
          for {
            _ <- Logger.println.info(s"=====| TestMain : $name |=====")
            result = Result.build(lexerInput, grammarInput)
            resultFrag = Result.resultToHTML(result)
            resultString = resultFrag.render
            outputFile <- File.fromPath("target/test-output.html")
            _ <- outputFile.writeString(resultString)
          } yield ()
        },
    )

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
      grammarInput = grammar("Lines")(
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

  override val executable: Executable =
    Executable.fromSubCommands(
      calc,
      simpleExpr,
    )

}
