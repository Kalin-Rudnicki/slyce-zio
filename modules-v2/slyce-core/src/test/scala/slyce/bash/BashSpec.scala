package slyce.bash

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.bash.model.*
import slyce.core.*
import slyce.core.builtIn.*

object BashSpec extends OxygenSpecDefault {

  private def bare(s: Source, text: String, from: Int = 0): BareWord =
    BareWord(text, spanOf(s, text, from))

  private def cmd(name: Word, args: ElementList[Word], redir: ElementOption[Redirect]): model.Command =
    model.Command(name, args, redir)

  private def pipelineStmt(s: Source, pipeline: Pipeline, semi: ElementOption[`;`]): PipelineStmt =
    PipelineStmt(pipeline, semi)

  override def testSpec: TestSpec =
    (
    suite("BashSpec")(
      suite("valid")(
        parsesTo(Script.parser, "") { s =>
          Script(elementList[Statement](eofSpan(s))())
        },
        parsesTo(Script.parser, "echo") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(bare(s, "echo"), elementList[Word](eofSpan(s))(), eoNone(eofSpan(s))),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "echo hello") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "echo"),
                    elementList[Word](eofSpan(s))(bare(s, "hello")),
                    eoNone(eofSpan(s)),
                  ),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "echo hello world") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "echo"),
                    elementList[Word](eofSpan(s))(bare(s, "hello"), bare(s, "world")),
                    eoNone(eofSpan(s)),
                  ),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "ls|grep txt") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(bare(s, "ls"), elementList[Word](spanOf(s, "|"))(), eoNone(spanOf(s, "|"))),
                  elementList[PipeCmd](eofSpan(s))(
                    PipeCmd(
                      Pipe("|", spanOf(s, "|")),
                      cmd(
                        bare(s, "grep"),
                        elementList[Word](eofSpan(s))(bare(s, "txt")),
                        eoNone(eofSpan(s)),
                      ),
                    ),
                  ),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "cat a|sort|uniq") { s =>
          val p0 = s.text.indexOf('|')
          val p1 = s.text.indexOf('|', p0 + 1)
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "cat"),
                    elementList[Word](span(s, p0, p0))(bare(s, "a")),
                    eoNone(span(s, p0, p0)),
                  ),
                  elementList[PipeCmd](eofSpan(s))(
                    PipeCmd(
                      Pipe("|", span(s, p0, p0 + 1)),
                      cmd(bare(s, "sort"), elementList[Word](span(s, p1, p1))(), eoNone(span(s, p1, p1))),
                    ),
                    PipeCmd(
                      Pipe("|", span(s, p1, p1 + 1)),
                      cmd(bare(s, "uniq"), elementList[Word](eofSpan(s))(), eoNone(eofSpan(s))),
                    ),
                  ),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "NAME=value") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              AssignStmt(
                Ident("NAME", spanOf(s, "NAME")),
                `=`("=", spanOf(s, "=")),
                bare(s, "value"),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "NAME=value;") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              AssignStmt(
                Ident("NAME", spanOf(s, "NAME")),
                `=`("=", spanOf(s, "=")),
                bare(s, "value"),
                eoSome(`;`( ";", spanOf(s, ";"))),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "echo $HOME") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "echo"),
                    elementList[Word](eofSpan(s))(
                      VarRef(`$`("$", spanOf(s, "$")), Ident("HOME", spanOf(s, "HOME"))),
                    ),
                    eoNone(eofSpan(s)),
                  ),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "echo \"hi\"") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "echo"),
                    elementList[Word](eofSpan(s))(
                      DoubleQuoted(
                        `"`("\"", span(s, 5, 6)),
                        elementList[DQuotePart](span(s, 8, 8))(
                          DQuoteChars("hi", span(s, 6, 8)),
                        ),
                        `"`("\"", span(s, 8, 9)),
                      ),
                    ),
                    eoNone(eofSpan(s)),
                  ),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "echo 'hi'") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "echo"),
                    elementList[Word](eofSpan(s))(
                      SingleQuoted(
                        `'`("'", span(s, 5, 6)),
                        SQuoteChars("hi", span(s, 6, 8)),
                        `'`("'", span(s, 8, 9)),
                      ),
                    ),
                    eoNone(eofSpan(s)),
                  ),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "echo hi>out") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "echo"),
                    elementList[Word](spanOf(s, ">"))(bare(s, "hi")),
                    eoSome(RedirOut(`>`(">", spanOf(s, ">")), bare(s, "out"))),
                  ),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "cat<input") { s =>
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "cat"),
                    elementList[Word](spanOf(s, "<"))(),
                    eoSome(RedirIn(`<`("<", spanOf(s, "<")), bare(s, "input"))),
                  ),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
        parsesTo(Script.parser, "echo a;echo b") { s =>
          val semi = s.text.indexOf(';')
          Script(
            elementList[Statement](eofSpan(s))(
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "echo"),
                    elementList[Word](span(s, semi, semi))(bare(s, "a")),
                    eoNone(span(s, semi, semi)),
                  ),
                  elementList[PipeCmd](span(s, semi, semi))(),
                ),
                eoSome(`;`( ";", span(s, semi, semi + 1))),
              ),
              pipelineStmt(
                s,
                Pipeline(
                  cmd(
                    bare(s, "echo", from = semi),
                    elementList[Word](eofSpan(s))(bare(s, "b")),
                    eoNone(eofSpan(s)),
                  ),
                  elementList[PipeCmd](eofSpan(s))(),
                ),
                eoNone(eofSpan(s)),
              ),
            ),
          )
        },
      ),
      suite("invalid")(
        failsToParse(Script.parser, "|"),
        failsToParse(Script.parser, "echo|"),
        failsToParse(Script.parser, "|echo"),
        failsToParse(Script.parser, "NAME="),
        failsToParse(Script.parser, "=value"),
        failsToParse(Script.parser, "echo \""),
        failsToParse(Script.parser, "echo '"),
        failsToParse(Script.parser, "echo $"),
        failsToParse(Script.parser, "echo >"),
        failsToParse(Script.parser, "echo <"),
        failsToParse(Script.parser, ";"),
        failsToParse(Script.parser, "echo a|"),
      ),
    )) @@ TestAspect.ignore // calculator e2e isolation


}
