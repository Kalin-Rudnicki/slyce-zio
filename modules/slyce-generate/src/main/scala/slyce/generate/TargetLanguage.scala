package slyce.generate

import cats.syntax.option.*
import harness.core.Enum

enum TargetLanguage extends Enum[TargetLanguage] { case Scala3 }
object TargetLanguage extends Enum.Companion[TargetLanguage] {

  def parse(targetLanguage: Option[TargetLanguage], extName: Option[String]): Option[TargetLanguage] =
    targetLanguage.orElse {
      extName match {
        case Some("scala") => TargetLanguage.Scala3.some
        case _             => None
      }
    }

  def extName(targetLanguage: TargetLanguage): String =
    targetLanguage match {
      case TargetLanguage.Scala3 => "scala"
    }

}
