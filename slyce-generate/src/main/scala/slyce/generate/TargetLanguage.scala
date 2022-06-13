package slyce.generate

import cats.syntax.option.*

enum TargetLanguage {
  case Scala3
}
object TargetLanguage {

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
