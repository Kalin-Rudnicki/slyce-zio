package slyce.generate.output.formatters

import slyce.generate.TargetLanguage
import slyce.generate.output.Result

trait Formatter {
  def format(pkg: List[String], name: String, result: Result): String
}
object Formatter {

  def format(targetLanguage: TargetLanguage, pkg: List[String], name: String, result: Result): String =
    targetLanguage match {
      case TargetLanguage.Scala3 => scala3.Scala3Formatter.format(pkg, name, result)
    }

}
