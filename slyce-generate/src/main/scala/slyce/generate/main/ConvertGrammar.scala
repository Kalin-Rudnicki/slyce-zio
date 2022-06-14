package slyce.generate.main

import cats.syntax.option.*

import slyce.core.*
import slyce.generate.grammar.*
import slyce.generate.parsers.Grammar as CurrentGrammar

object ConvertGrammar {

  def convertGrammar(grammar: CurrentGrammar.NonTerminal.Grammar): GrammarInput =
    GrammarInput(
      startNT = grammar._2.markedText,
      nonTerminals = grammar._3.toNonEmptyList.toList.map(convertNT),
      maxLookAhead = Marked(3, Span.Unknown), // TODO (KR) : Add ability to specify in file
    )

  private def convertNT(nt: CurrentGrammar.NonTerminal.NT): GrammarInput.NamedNonTerminal =
    GrammarInput.NamedNonTerminal(
      name = nt._1.markedText.map(GrammarInput.Identifier.NonTerminal(_)),
      nonTerminal = converNTBody(nt._2),
    )

  private def converNTBody(ntBody: CurrentGrammar.NonTerminal.NTBody): GrammarInput.NonTerminal =
    ntBody.lift match {
      case standardNT: CurrentGrammar.NonTerminal.StandardNT => convertStandardNT(standardNT)
      case listNT: CurrentGrammar.NonTerminal.ListNT         => convertListNT(listNT)
      case assocNT: CurrentGrammar.NonTerminal.AssocNT       => convertAssocNT(assocNT)
    }

  private def convertStandardNT(standardNT: CurrentGrammar.NonTerminal.StandardNT): GrammarInput.NonTerminal.StandardNonTerminal =
    standardNT match {
      case CurrentGrammar.NonTerminal.StandardNT._1(basicNT) =>
        GrammarInput.NonTerminal.StandardNonTerminal.`:`(
          productions = basicNT._2.toNonEmptyList.map(convertElementList),
        )
      case CurrentGrammar.NonTerminal.StandardNT._2(liftNT) =>
        GrammarInput.NonTerminal.StandardNonTerminal.^(
          productions = liftNT._2.toNonEmptyList.map(convertLiftElementList),
        )
    }

  private def convertListNT(listNT: CurrentGrammar.NonTerminal.ListNT): GrammarInput.NonTerminal.ListNonTerminal =
    listNT match {
      case CurrentGrammar.NonTerminal.ListNT._1(listType, start) =>
        GrammarInput.NonTerminal.ListNonTerminal(
          `type` = convertListType(listType),
          start = convertLiftElementList(start),
          repeat = None,
        )
      case CurrentGrammar.NonTerminal.ListNT._2(listType, start, _, repeat) =>
        GrammarInput.NonTerminal.ListNonTerminal(
          `type` = convertListType(listType),
          start = convertLiftElementList(start),
          repeat = convertLiftElementList(repeat).some,
        )
    }

  private def convertAssocNT(assocNT: CurrentGrammar.NonTerminal.AssocNT): GrammarInput.NonTerminal.AssocNonTerminal =
    GrammarInput.NonTerminal.AssocNonTerminal(
      assocElements = assocNT._2.toNonEmptyList.map(connertAssocPair),
      base = convertStandardNT(assocNT._3),
    )

  private def connertAssocPair(
      assocPair: CurrentGrammar.NonTerminal.AssocPair,
  ): (Marked[GrammarInput.NonTerminal.AssocNonTerminal.Type], Marked[GrammarInput.Element]) =
    (
      convertAssocType(assocPair._1),
      convertElement(assocPair._2),
    )

  private def convertAssocType(assocType: CurrentGrammar.NonTerminal.AssocType): Marked[GrammarInput.NonTerminal.AssocNonTerminal.Type] =
    assocType.lift match {
      case CurrentGrammar.Terminal.`<`(span) => Marked(GrammarInput.NonTerminal.AssocNonTerminal.Type.Left, span)
      case CurrentGrammar.Terminal.`>`(span) => Marked(GrammarInput.NonTerminal.AssocNonTerminal.Type.Right, span)
    }

  private def convertListType(listType: CurrentGrammar.NonTerminal.ListType): GrammarInput.NonTerminal.ListNonTerminal.Type =
    listType.lift match {
      case _: CurrentGrammar.Terminal.`*` => GrammarInput.NonTerminal.ListNonTerminal.Type.*
      case _: CurrentGrammar.Terminal.`+` => GrammarInput.NonTerminal.ListNonTerminal.Type.+
    }

  private def convertElementList(elementList: CurrentGrammar.NonTerminal.ElementList): List[Marked[GrammarInput.Element]] =
    elementList.toList.map(convertElement)

  private def convertLiftElementList(liftElementList: CurrentGrammar.NonTerminal.LiftElementList): LiftList[Marked[GrammarInput.Element]] =
    liftElementList match {
      case CurrentGrammar.NonTerminal.LiftElementList._1(lift) =>
        LiftList(Nil, convertElement(lift), Nil)
      case CurrentGrammar.NonTerminal.LiftElementList._2(before, _, lift, after) =>
        LiftList(convertElementList(before), convertElement(lift), convertElementList(after))
    }

  private def convertElement(element: CurrentGrammar.NonTerminal.Element): Marked[GrammarInput.Element] = {
    val (e, h) = convertElementH(element)
    Marked(e, h)
  }

  private def convertElementH(element: CurrentGrammar.NonTerminal.Element): (GrammarInput.Element, Span.Highlight) = {
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

  private def convertNonOptElement(element: CurrentGrammar.NonTerminal.NonOptElement): (GrammarInput.Element.NonOptional, Span.Highlight) =
    element.lift match {
      case CurrentGrammar.Terminal.nonTerminal(text, span) => (GrammarInput.Identifier.NonTerminal(text), span)
      case CurrentGrammar.Terminal.terminal(text, span)    => (GrammarInput.Identifier.Terminal(text), span)
      case anonList: CurrentGrammar.NonTerminal.AnonList   => convertAnonList(anonList)
      case raw: CurrentGrammar.NonTerminal.Raw             => convertRaw(raw)
    }

  private def convertAnonList(anonList: CurrentGrammar.NonTerminal.AnonList): (GrammarInput.NonTerminal.ListNonTerminal, Span.Highlight) =
    anonList match {
      case CurrentGrammar.NonTerminal.AnonList._1(element, listType) =>
        val (e, h) = convertElementH(element)
        (
          GrammarInput.NonTerminal.ListNonTerminal(
            `type` = convertListType(listType),
            start = LiftList(Nil, Marked(e, h), Nil),
            repeat = None,
          ),
          h <> listType.lift.span,
        )
      case CurrentGrammar.NonTerminal.AnonList._2(p1, start, _, listType) =>
        (
          GrammarInput.NonTerminal.ListNonTerminal(
            `type` = convertListType(listType),
            start = convertLiftElementList(start),
            repeat = None,
          ),
          p1.span <> listType.lift.span,
        )
      case CurrentGrammar.NonTerminal.AnonList._3(p1, start, _, repeat, _, listType) =>
        (
          GrammarInput.NonTerminal.ListNonTerminal(
            `type` = convertListType(listType),
            start = convertLiftElementList(start),
            repeat = convertLiftElementList(repeat).some,
          ),
          p1.span <> listType.lift.span,
        )
    }

  private def convertRaw(raw: CurrentGrammar.NonTerminal.Raw): (GrammarInput.Identifier.Raw, Span.Highlight) =
    (
      GrammarInput.Identifier.Raw(
        raw._2.toNonEmptyList.toList.map {
          case CurrentGrammar.NonTerminal.Char._1(chars) => chars.text
          case CurrentGrammar.NonTerminal.Char._2(escChar) =>
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
