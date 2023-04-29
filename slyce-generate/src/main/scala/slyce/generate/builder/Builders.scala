package slyce.generate.builder

import cats.data.NonEmptyList
import cats.syntax.option.*
import zio.Trace
import zio.internal.stacktracer.Tracer

import slyce.core.*
import slyce.generate.grammar.*
import slyce.generate.lexer.*

object Builders {

  extension [A](a: A) {
    def markedUnknown: Marked[A] = Marked(a, Span.Unknown)
  }
  extension (nt: GrammarInput.NonTerminal) {
    def apply(name: String): GrammarInput.NamedNonTerminal =
      GrammarInput.NamedNonTerminal(
        GrammarInput.Identifier.NonTerminal(name).markedUnknown,
        nt,
      )
  }
  extension (nonOpt: grammar.NonOptElem) {
    def optional: GrammarInput.Element =
      GrammarInput.Element.Optional(grammar.NonOptElem(nonOpt))
  }

  object lexer {

    def apply(startMode: String)(modes: LexerInput.Mode*): LexerInput =
      LexerInput(
        startMode.markedUnknown,
        modes.toList,
      )

    object mode {

      def apply(modeName: String)(lines: LexerInput.Mode.Line*): LexerInput.Mode =
        LexerInput.Mode(
          modeName.markedUnknown,
          lines.toList,
        )

      def line(
          reg: Regex,
          toMode: Yields.ToMode[String] = Yields.ToMode.Same,
      )(
          yields: Yields.Yield*,
      )(using trace: Trace): LexerInput.Mode.Line =
        LexerInput.Mode.Line(
          Tracer.instance.unapply(trace).get._3,
          reg.markedUnknown,
          Yields(
            yields.toList.map(_.markedUnknown),
            toMode.markedUnknown,
          ),
        )

    }

  }

  object grammar {

    def apply(startNT: String, maxLookAhead: Int = 1)(
        nts: GrammarInput.NamedNonTerminal*,
    ): GrammarInput =
      GrammarInput(
        startNT = startNT.markedUnknown,
        nonTerminals = nts.toList,
        maxLookAhead = Marked(maxLookAhead, Span.Unknown),
      )

    type Elem = String | GrammarInput.Element
    object Elem {

      def apply(eb: Elem): GrammarInput.Element =
        eb match {
          case str: String             => GrammarInput.Identifier(str)
          case e: GrammarInput.Element => e
        }

    }

    type NonOptElem = String | GrammarInput.Element.NonOptional
    object NonOptElem {

      def apply(eb: NonOptElem): GrammarInput.Element.NonOptional =
        eb match {
          case str: String                         => GrammarInput.Identifier(str)
          case e: GrammarInput.Element.NonOptional => e
        }

    }

    def liftElements(before: Elem*)(lift: Elem)(after: Elem*): LiftList[Marked[GrammarInput.Element]] =
      LiftList(before.toList, lift, after.toList).map(Elem(_).markedUnknown)

    def elements(elems: Elem*): List[Marked[GrammarInput.Element]] =
      elems.toList.map(Elem(_).markedUnknown)

    object nt {

      sealed abstract class ListNTBuilder(listType: GrammarInput.NonTerminal.ListNonTerminal.Type) {

        def apply(start: LiftList[Marked[GrammarInput.Element]]): GrammarInput.NonTerminal.ListNonTerminal =
          GrammarInput.NonTerminal.ListNonTerminal(
            listType,
            start,
            None,
          )
        def apply(start: LiftList[Marked[GrammarInput.Element]], repeat: LiftList[Marked[GrammarInput.Element]]): GrammarInput.NonTerminal.ListNonTerminal =
          GrammarInput.NonTerminal.ListNonTerminal(
            listType,
            start,
            repeat.some,
          )

      }

      def `:`(
          elements0: List[Marked[GrammarInput.Element]],
          elementsN: List[Marked[GrammarInput.Element]]*,
      ): GrammarInput.NonTerminal.StandardNonTerminal.`:` =
        GrammarInput.NonTerminal.StandardNonTerminal.`:`(NonEmptyList(elements0, elementsN.toList))

      def ^(
          elements0: LiftList[Marked[GrammarInput.Element]],
          elementsN: LiftList[Marked[GrammarInput.Element]]*,
      ): GrammarInput.NonTerminal.StandardNonTerminal.^ =
        GrammarInput.NonTerminal.StandardNonTerminal.^(NonEmptyList(elements0, elementsN.toList))

      object * extends ListNTBuilder(GrammarInput.NonTerminal.ListNonTerminal.Type.*)
      object + extends ListNTBuilder(GrammarInput.NonTerminal.ListNonTerminal.Type.+)

      def ~(
          assoc0: Either[Elem, Elem],
          assocN: Either[Elem, Elem]*,
      )(
          base: GrammarInput.NonTerminal.StandardNonTerminal,
      ): GrammarInput.NonTerminal.AssocNonTerminal =
        GrammarInput.NonTerminal.AssocNonTerminal(
          assocElements = NonEmptyList(assoc0, assocN.toList).map {
            case Left(value)  => (GrammarInput.NonTerminal.AssocNonTerminal.Type.Left.markedUnknown, Elem(value).markedUnknown)
            case Right(value) => (GrammarInput.NonTerminal.AssocNonTerminal.Type.Right.markedUnknown, Elem(value).markedUnknown)
          },
          base = base,
        )

    }

  }

}
