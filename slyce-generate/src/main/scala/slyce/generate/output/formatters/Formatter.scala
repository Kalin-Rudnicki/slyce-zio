package slyce.generate.output.formatters

import slyce.generate.output.Result

trait Formatter {
  def format(pkg: List[String], name: String, result: Result): String
}