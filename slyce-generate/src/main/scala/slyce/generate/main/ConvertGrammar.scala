package slyce.generate.main

import cats.syntax.option.*

import slyce.core.*
import slyce.generate.grammar.*
import slyce.generate.parsers.Grammar
import slyce.generate.parsers.Grammar.Terminal

object ConvertGrammar {

  def convertGrammar(grammar: Grammar.NonTerminal.Grammar): GrammarInput =
    GrammarInput(
      startNT = grammar._2.markedText,
      nonTerminals = grammar._3.toNonEmptyList.toList.map(convertNT),
      maxLookAhead = Marked(3, Span.Unknown), // TODO (KR) : Add ability to specify in file
    )

  private def convertNT(nt: Grammar.NonTerminal.NT): GrammarInput.NamedNonTerminal =
    GrammarInput.NamedNonTerminal(
      name = nt._1.markedText.map(GrammarInput.Identifier.NonTerminal(_)),
      nonTerminal = converNTBody(nt._2),
    )

  private def converNTBody(ntBody: Grammar.NonTerminal.NTBody): GrammarInput.NonTerminal =
    ntBody.lift match {
      case standardNT: Grammar.NonTerminal.StandardNT => convertStandardNT(standardNT)
      case listNT: Grammar.NonTerminal.ListNT         => convertListNT(listNT)
      case assocNT: Grammar.NonTerminal.AssocNT       => convertAssocNT(assocNT)
    }

  private def convertStandardNT(standardNT: Grammar.NonTerminal.StandardNT): GrammarInput.NonTerminal.StandardNonTerminal =
    standardNT match {
      case Grammar.NonTerminal.StandardNT._1(basicNT) =>
        GrammarInput.NonTerminal.StandardNonTerminal.`:`(
          productions = basicNT._2.toNonEmptyList.map(convertElementList),
        )
      case Grammar.NonTerminal.StandardNT._2(liftNT) =>
        GrammarInput.NonTerminal.StandardNonTerminal.^(
          productions = liftNT._2.toNonEmptyList.map(convertLiftElementList),
        )
    }

  private def convertListNT(listNT: Grammar.NonTerminal.ListNT): GrammarInput.NonTerminal.ListNonTerminal =
    listNT match {
      case Grammar.NonTerminal.ListNT._1(listType, start) =>
        GrammarInput.NonTerminal.ListNonTerminal(
          `type` = convertListType(listType),
          start = convertLiftElementList(start),
          repeat = None,
        )
      case Grammar.NonTerminal.ListNT._2(listType, start, _, repeat) =>
        GrammarInput.NonTerminal.ListNonTerminal(
          `type` = convertListType(listType),
          start = convertLiftElementList(start),
          repeat = convertLiftElementList(repeat).some,
        )
    }

  private def convertAssocNT(assocNT: Grammar.NonTerminal.AssocNT): GrammarInput.NonTerminal.AssocNonTerminal =
    GrammarInput.NonTerminal.AssocNonTerminal(
      assocElements = assocNT._2.toNonEmptyList.map(connertAssocPair),
      base = convertStandardNT(assocNT._3),
    )

  private def connertAssocPair(
      assocPair: Grammar.NonTerminal.AssocPair,
  ): (Marked[GrammarInput.NonTerminal.AssocNonTerminal.Type], Marked[GrammarInput.Element]) =
    (
      convertAssocType(assocPair._1),
      convertElement(assocPair._2),
    )

  private def convertAssocType(assocType: Grammar.NonTerminal.AssocType): Marked[GrammarInput.NonTerminal.AssocNonTerminal.Type] =
    assocType.lift match {
      case Grammar.Terminal.`<`(span) => Marked(GrammarInput.NonTerminal.AssocNonTerminal.Type.Left, span)
      case Grammar.Terminal.`>`(span) => Marked(GrammarInput.NonTerminal.AssocNonTerminal.Type.Right, span)
    }

  private def convertListType(listType: Grammar.NonTerminal.ListType): GrammarInput.NonTerminal.ListNonTerminal.Type =
    listType.lift match {
      case _: Grammar.Terminal.`*` => GrammarInput.NonTerminal.ListNonTerminal.Type.*
      case _: Grammar.Terminal.`+` => GrammarInput.NonTerminal.ListNonTerminal.Type.+
    }

  private def convertElementList(elementList: Grammar.NonTerminal.ElementList): List[Marked[GrammarInput.Element]] =
    elementList.toList.map(convertElement)

  private def convertLiftElementList(liftElementList: Grammar.NonTerminal.LiftElementList): LiftList[Marked[GrammarInput.Element]] =
    liftElementList match {
      case Grammar.NonTerminal.LiftElementList._1(lift) =>
        LiftList(Nil, convertElement(lift), Nil)
      case Grammar.NonTerminal.LiftElementList._2(before, _, lift, after) =>
        LiftList(convertElementList(before), convertElement(lift), convertElementList(after))
    }

  private def convertElement(element: Grammar.NonTerminal.Element): Marked[GrammarInput.Element] = {
    val (e, h) = convertElementH(element)
    Marked(e, h)
  }

  private def convertElementH(element: Grammar.NonTerminal.Element): (GrammarInput.Element, Span.Highlight) = {
    val (nonOpt, h) = convertNonOptElement(element._1)

    element._2.toOption match {
      case Some(q) =>
        (
          GrammarInput.Element.Optional(nonOpt),
          Span.Highlight(h.start, q.span.end, h.source),
        )
      case None =>
        (nonOpt, h)
    }
  }

  private def convertNonOptElement(element: Grammar.NonTerminal.NonOptElement): (GrammarInput.Element.NonOptional, Span.Highlight) =
    element.lift match {
      case Grammar.Terminal.nonTerminal(text, span) => (GrammarInput.Identifier.NonTerminal(text), span)
      case Grammar.Terminal.terminal(text, span)    => (GrammarInput.Identifier.Terminal(text), span)
      case anonList: Grammar.NonTerminal.AnonList   => convertAnonList(anonList)
      case raw: Grammar.NonTerminal.Raw             => convertRaw(raw)
    }

  private def convertAnonList(anonList: Grammar.NonTerminal.AnonList): (GrammarInput.NonTerminal.ListNonTerminal, Span.Highlight) =
    anonList match {
      case Grammar.NonTerminal.AnonList._1(element, listType) =>
        val (e, h) = convertElementH(element)
        (
          GrammarInput.NonTerminal.ListNonTerminal(
            `type` = convertListType(listType),
            start = LiftList(Nil, Marked(e, h), Nil),
            repeat = None,
          ),
          h <> listType.lift.span,
        )
      case Grammar.NonTerminal.AnonList._2(p1, start, _, listType) =>
        (
          GrammarInput.NonTerminal.ListNonTerminal(
            `type` = convertListType(listType),
            start = convertLiftElementList(start),
            repeat = None,
          ),
          p1.span <> listType.lift.span,
        )
      case Grammar.NonTerminal.AnonList._3(p1, start, _, repeat, _, listType) =>
        (
          GrammarInput.NonTerminal.ListNonTerminal(
            `type` = convertListType(listType),
            start = convertLiftElementList(start),
            repeat = convertLiftElementList(repeat).some,
          ),
          p1.span <> listType.lift.span,
        )
    }

  private def convertRaw(raw: Grammar.NonTerminal.Raw): (GrammarInput.Identifier.Raw, Span.Highlight) =
    (
      GrammarInput.Identifier.Raw(
        raw._2.toNonEmptyList.toList.map {
          case Grammar.NonTerminal.Char._1(chars) => chars.text
          case Grammar.NonTerminal.Char._2(escChar) =>
            escChar.text.head match {
              case 'n'  => "\n"
              case 't'  => "\t"
              case '\\' => "\\"
              case c    => c.toString
            }
        }.mkString,
      ),
      Span.Highlight(raw._1.span.start, raw._3.span.end, raw._1.span.source),
    )

}
