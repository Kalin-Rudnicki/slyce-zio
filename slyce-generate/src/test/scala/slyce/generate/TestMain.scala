package slyce.generate

import cats.syntax.either.*
import cats.syntax.option.*
import klib.utils.{given, *}
import klib.utils.commandLine.parse.*
import zio.*

import slyce.core.*
import slyce.generate.builder.Builders.*
import slyce.generate.debugging.Result as DebugResult
import slyce.generate.grammar.*
import slyce.generate.lexer.*
import slyce.generate.output.*

object TestMain extends ExecutableApp {

  private[generate] def makeExe(
      name: String,
      lexerInput: LexerInput,
      grammarInput: GrammarInput,
      baseDir: List[String] = List(".", "slyce-generate", "src", "test", "scala"),
      pkg: List[String] = List("slyce", "generate", "test"),
  ): (String, Executable) =
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

            refName = name.split("-").map(_.capitalize).mkString
            result <- ZIO.fromEither(Result.build(lexerInput, grammarInput).leftMap(_.map(e => KError.UserError(e.toString))))
            resultString = formatters.scala3.Scala3Formatter.format(pkg, refName, result)
            // _ <- Logger.println.info(resultString)
            _ <- Logger.println.info(s"Generated ${resultString.count(_ == '\n')} line(s)")

            _ <- Logger.break()
            dirPath = (baseDir ::: pkg).mkString("/")
            dirFile <- File.fromPath(dirPath)
            _ <- dirFile.createDirectories()
            outFile <- dirFile.child(s"$refName.scala")
            _ <- Logger.println.info(s"writing to: ${outFile.toJavaFile.getCanonicalPath}")
            _ <- outFile.writeString(resultString)
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

  private val tmp: (String, Executable) =
    makeExe(
      "tmp",
      lexerInput = lexer("General")(
        lexer.mode("General")(
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[a-z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("variable")),
          lexer.mode.line(Regex.CharClass.inclusive('[', ']', ','))(Yields.Yield.Text()),
          lexer.mode.line(Regex.CharClass.inclusive(' ', '\t', '\n'))(),
        ),
      ),
      grammarInput = grammar("Root")(
        grammar.nt.`:`(
          grammar.elements(
            "[",
            grammar.nt.*(
              grammar.liftElements()("variable")(),
              grammar.liftElements(",")("variable")(),
            ),
            "]",
          ),
        )("Root"),
      ),
    )

  // =====| Bootstrap |=====

  private val parsersDir =
    List(".", "slyce-generate", "src", "main", "scala")

  private val parsersPkg =
    List("slyce", "generate", "parsers")

  // =====| Lexer |=====

  private val lexerGen: (String, Executable) =
    makeExe(
      "lexer",
      lexerInput = lexer("General")(
        lexer.mode("General")(
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('@'),
              Regex.Group(
                Regex.Sequence("start"),
                Regex.Sequence("mode"),
              ),
              Regex.CharClass.inclusive(':'),
            ),
            Yields.ToMode.Push("Mode"),
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('/').exactlyN(2),
              Regex.CharClass.exclusive('\n').anyAmount,
              Regex.CharClass.inclusive('\n'),
            ),
          )(),
          // TODO (KR) : Block comments
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('\\'),
              Regex.CharClass.inclusive('.', '/', '@', ';', 'n', 't', '\\', '[', ']', '(', '|', ')', '{', '}', '?', '*', '+'),
            ),
          )(Yields.Yield.Terminal("escChar", subString = (1.some, 1.some))),
          lexer.mode.line(Regex.CharClass.inclusive('\n'))(),
          lexer.mode.line(
            Regex.CharClass.inclusive(';'),
            Yields.ToMode.Push("LineEnd"),
          )(Yields.Yield.Text()),
          lexer.mode.line(Regex.CharClass.inclusive('(', '|', ')', '?', '*', '+'))(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.CharClass.inclusive('['),
            Yields.ToMode.Push("CharClass"),
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.CharClass.inclusive('{'),
            Yields.ToMode.Push("Quant"),
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.Sequence("\\d"),
          )(Yields.Yield.Terminal("escChars", subString = (1.some, 1.some))),
          lexer.mode.line(Regex.CharClass.inclusive('.'))(Yields.Yield.Terminal("escChars")),
          lexer.mode.line(Regex.CharClass.exclusive('\\'))(Yields.Yield.Terminal("char")),
          lexer.mode.line(Regex.CharClass.inclusive(' ').atLeastN(2))(),
        ),
        lexer.mode("Mode")(
          lexer.mode.line(Regex.CharClass.inclusive(' ', '\t').atLeastOnce)(),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[A-Z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("mode")),
          lexer.mode.line(
            Regex.CharClass.inclusive('\n'),
            Yields.ToMode.Pop,
          )(),
        ),
        lexer.mode("String")(
          lexer.mode.line(
            Regex.CharClass.inclusive('"'),
            Yields.ToMode.Pop,
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('\\'),
              Regex.CharClass.inclusive('\\', 'n', 't', '"'),
            ),
          )(Yields.Yield.Terminal("escChar", subString = (1.some, 1.some))),
          lexer.mode.line(Regex.CharClass.exclusive('\\', '"').atLeastOnce)(Yields.Yield.Terminal("chars")),
        ),
        lexer.mode("LineEnd")(
          lexer.mode.line(Regex.CharClass.inclusive(' ', '\t').atLeastOnce)(),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('/').exactlyN(2),
              Regex.CharClass.exclusive('\n').anyAmount,
              Regex.CharClass.inclusive('\n'),
            ),
            Yields.ToMode.Pop,
          )(),
          // TODO (KR) : Block Comment
          lexer.mode.line(
            Regex.Group(
              Regex.Sequence(Regex.CharClass.inclusive('[', ']', ',', '@')),
              Regex.Sequence(">>"),
              Regex.Sequence("->"),
              Regex.Sequence("<-"),
            ),
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('-').optional,
              Regex.CharClass.`\\d`.atLeastOnce,
            ),
          )(Yields.Yield.Terminal("int")),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[A-Z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("mode")),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[a-z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("term")),
          lexer.mode.line(
            Regex.CharClass.inclusive('\n'),
            Yields.ToMode.Pop,
          )(),
        ),
        lexer.mode("CharClass")(
          lexer.mode.line(
            Regex.CharClass.inclusive(']'),
            Yields.ToMode.Pop,
          )(Yields.Yield.Text()),
          lexer.mode.line(Regex.CharClass.inclusive('^', '-'))(Yields.Yield.Text()),
          lexer.mode.line(Regex.Sequence("\\d"))(Yields.Yield.Terminal("escChars", subString = (1.some, 1.some))),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('\\'),
              Regex.CharClass.exclusive(),
            ),
          )(Yields.Yield.Terminal("escChar", subString = (1.some, 1.some))),
          lexer.mode.line(Regex.CharClass.exclusive('\\'))(Yields.Yield.Terminal("char")),
        ),
        lexer.mode("Quant")(
          lexer.mode.line(Regex.CharClass.inclusive(' ', '\t').atLeastOnce)(),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('-').optional,
              Regex.CharClass.`\\d`.atLeastOnce,
            ),
          )(Yields.Yield.Terminal("int")),
          lexer.mode.line(Regex.CharClass.inclusive(','))(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.CharClass.inclusive('}'),
            Yields.ToMode.Pop,
          )(Yields.Yield.Text()),
        ),
      ),
      grammarInput = grammar("Lexer", 2)(
        grammar.nt.`:`(
          grammar.elements(
            "@start:",
            "mode",
            grammar.nt.+(
              grammar.liftElements()("Mode")(),
            ),
          ),
        )("Lexer"),
        grammar.nt.`:`(
          grammar.elements(
            "@mode:",
            "mode",
            grammar.nt.+(
              grammar.liftElements()("Line")(),
            ),
          ),
        )("Mode"),
        grammar.nt.`:`(
          grammar.elements(
            "GroupInner",
            ";",
            grammar.nt.*(
              grammar.liftElements()("Yield")(),
              grammar.liftElements(",")("Yield")(),
            ),
            "ToMode",
          ),
        )("Line"),
        grammar.nt.`:`(
          grammar.elements(
            "YieldType",
            "SubString",
          ),
        )("Yield"),
        grammar.nt.^(
          grammar.liftElements()("@")(),
          grammar.liftElements()("term")(),
          grammar.liftElements()("Raw")(),
        )("YieldType"),
        grammar.nt.`:`(
          grammar.elements(),
          grammar.elements("[", "int", "]"),
          grammar.elements("[", "int", ",", "]"),
          grammar.elements("[", ",", "int", "]"),
          grammar.elements("[", "int", ",", "int", "]"),
        )("SubString"),
        grammar.nt.`:`(
          grammar.elements(),
          grammar.elements("->", "mode"),
          grammar.elements(">>", "mode"),
          grammar.elements("<-"),
        )("ToMode"),
        grammar.nt.^(
          grammar.liftElements(
            "\"",
          )(
            grammar.nt.+(
              grammar.liftElements()("Char")(),
            ),
          )(
            "\"",
          ),
        )("Raw"),
        grammar.nt.^(
          grammar.liftElements()("chars")(),
          grammar.liftElements()("escChar")(),
        )("Char"),
        grammar.nt.`:`(
          grammar.elements("Group"),
          grammar.elements("CharClass"),
          grammar.elements("Regex", "Quant"),
        )("Regex"),
        grammar.nt.+(
          grammar.liftElements()("Sequence")(),
          grammar.liftElements("|")("Sequence")(),
        )("GroupInner"),
        grammar.nt.*(
          grammar.liftElements()("Regex")(),
        )("Sequence"),
        grammar.nt.^(
          grammar.liftElements("(")("GroupInner")(")"),
        )("Group"),
        grammar.nt.`:`(
          grammar.elements("?"),
          grammar.elements("*"),
          grammar.elements("+"),
          grammar.elements("{", "int", "}"),
          grammar.elements("{", "int", ",", "}"),
          grammar.elements("{", ",", "int", "}"),
          grammar.elements("{", "int", ",", "int", "}"),
        )("Quant"),
        grammar.nt.`:`(
          grammar.elements(
            "[",
            "^".optional,
            grammar.nt.+(
              grammar.liftElements()("CCChars")(),
            ),
            "]",
          ),
          grammar.elements("char"),
          grammar.elements("escChar"),
          grammar.elements("escChars"),
        )("CharClass"),
        grammar.nt.`:`(
          grammar.elements("CCChar", "-", "CCChar"),
          grammar.elements("CCChar"),
          grammar.elements("escChars"),
        )("CCChars"),
        grammar.nt.`:`(
          grammar.elements("char"),
          grammar.elements("escChar"),
        )("CCChar"),
      ),
      parsersDir,
      parsersPkg,
    )

  val grammarGen: (String, Executable) =
    makeExe(
      "grammar",
      lexerInput = lexer("General")(
        lexer.mode("General")(
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('/').exactlyN(2),
              Regex.CharClass.exclusive('\n').anyAmount,
              Regex.CharClass.inclusive('\n'),
            ),
          )(),
          // TODO (KR) : Block Comment
          lexer.mode.line(Regex.CharClass.inclusive(' ', '\t', '\n').atLeastOnce)(),
          lexer.mode.line(
            Regex.Sequence("@start:"),
            Yields.ToMode.Push("Mode"),
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[A-Z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("nonTerminal")),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[a-z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("terminal")),
          lexer.mode.line(
            Regex.CharClass.inclusive('"'),
            Yields.ToMode.Push("String"),
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.CharClass.inclusive(':', '^', '*', '+', '~', '<', '>'),
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.CharClass.inclusive(';', '(', ')', '|', '.', '?'),
          )(Yields.Yield.Text()),
        ),
        lexer.mode("Mode")(
          lexer.mode.line(Regex.CharClass.inclusive(' ', '\t').atLeastOnce)(),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.`[A-Z]`,
              Regex.CharClass.`[A-Za-z_\\d]`.anyAmount,
            ),
          )(Yields.Yield.Terminal("mode")),
          lexer.mode.line(
            Regex.CharClass.inclusive('\n'),
            Yields.ToMode.Pop,
          )(),
        ),
        lexer.mode("String")(
          lexer.mode.line(
            Regex.CharClass.inclusive('"'),
            Yields.ToMode.Pop,
          )(Yields.Yield.Text()),
          lexer.mode.line(
            Regex.Sequence(
              Regex.CharClass.inclusive('\\'),
              Regex.CharClass.inclusive('\\', 'n', 't', '"'),
            ),
          )(Yields.Yield.Terminal("escChar", subString = (1.some, 1.some))),
          lexer.mode.line(Regex.CharClass.exclusive('\\', '"').atLeastOnce)(Yields.Yield.Terminal("chars")),
        ),
      ),
      grammarInput = grammar("Grammar")(
        grammar.nt.`:`(
          grammar.elements(
            "@start:",
            "mode",
            grammar.nt.+(
              grammar.liftElements()("NT")(";"),
            ),
          ),
        )("Grammar"),
        grammar.nt.*(
          grammar.liftElements()("Element")(),
        )("ElementList"),
        grammar.nt.`:`(
          grammar.elements("Element"),
          grammar.elements("ElementList", "^", "Element", "ElementList"),
        )("LiftElementList"),
        grammar.nt.`:`(
          grammar.elements("nonTerminal", "NTBody"),
        )("NT"),
        grammar.nt.^(
          grammar.liftElements()("StandardNT")(),
          grammar.liftElements()("ListNT")(),
          grammar.liftElements()("AssocNT")(),
        )("NTBody"),
        grammar.nt.`:`(
          grammar.elements("BasicNT"),
          grammar.elements("LiftNT"),
        )("StandardNT"),
        grammar.nt.`:`(
          grammar.elements(
            ":",
            grammar.nt.+(
              grammar.liftElements()("ElementList")(),
              grammar.liftElements("|")("ElementList")(),
            ),
          ),
        )("BasicNT"),
        grammar.nt.`:`(
          grammar.elements(
            "^",
            grammar.nt.+(
              grammar.liftElements()("LiftElementList")(),
              grammar.liftElements("|")("LiftElementList")(),
            ),
          ),
        )("LiftNT"),
        grammar.nt.`:`(
          grammar.elements("ListType", "LiftElementList"),
          grammar.elements("ListType", "LiftElementList", ".", "LiftElementList"),
        )("ListNT"),
        grammar.nt.`^`(
          grammar.liftElements()("*")(),
          grammar.liftElements()("+")(),
        )("ListType"),
        grammar.nt.`:`(
          grammar.elements(
            "~",
            grammar.nt.+(
              grammar.liftElements()("AssocPair")(),
              grammar.liftElements("|")("AssocPair")(),
            ),
            "StandardNT",
          ),
        )("AssocNT"),
        grammar.nt.`:`(
          grammar.elements("AssocType", "Element"),
        )("AssocPair"),
        grammar.nt.^(
          grammar.liftElements()("<")(),
          grammar.liftElements()(">")(),
        )("AssocType"),
        grammar.nt.`:`(
          grammar.elements("NonOptElement", "?".optional),
        )("Element"),
        grammar.nt.`:`(
          grammar.elements("Element", "ListType"),
          grammar.elements("(", "LiftElementList", ")", "ListType"),
          grammar.elements("(", "LiftElementList", ".", "LiftElementList", ")", "ListType"),
        )("AnonList"),
        grammar.nt.^(
          grammar.liftElements()("nonTerminal")(),
          grammar.liftElements()("terminal")(),
          grammar.liftElements()("Raw")(),
          grammar.liftElements()("AnonList")(),
        )("NonOptElement"),
        grammar.nt.`:`(
          grammar.elements(
            "\"",
            grammar.nt.+(
              grammar.liftElements()("Char")(),
            ),
            "\"",
          ),
        )("Raw"),
        grammar.nt.^(
          grammar.liftElements()("chars")(),
          grammar.liftElements()("escChar")(),
        )("Char"),
      ),
      parsersDir,
      parsersPkg,
    )

  override val executable: Executable =
    Executable.fromSubCommands(
      calc,
      simpleExpr,
      causeConflict,
      tmp,
      lexerGen,
      grammarGen,
    )

}
