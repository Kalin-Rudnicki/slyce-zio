package slyce.generate.lexer

import cats.syntax.either.*
import cats.syntax.option.*
import zio.test.*
import zio.test.Assertion.*

import slyce.core.*
import slyce.generate.builder.Builders.*

def testNFA(name: String)(lexer: LexerInput, expNumErrors: Option[Int]): ZSpec[Any, Any] =
  test(name) {
    val res = NFA.fromLexer(lexer)

    expNumErrors match {
      case Some(expNumErrors) => assert(res.leftMap(_.toList))(isLeft(hasSize(equalTo(expNumErrors))))
      case None               => assert(res)(isRight)
    }
  }

def testDFA(name: String)(lexer: LexerInput, expNumErrors: Option[Int]): ZSpec[Any, Any] =
  test(name) {
    val res = NFA.fromLexer(lexer).flatMap(DFA.fromNFA(_))

    expNumErrors match {
      case Some(expNumErrors) => assert(res.leftMap(_.toList))(isLeft(hasSize(equalTo(expNumErrors))))
      case None               => assert(res)(isRight)
    }
  }
