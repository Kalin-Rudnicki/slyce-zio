package slyce.test.json

object Conversion {

  private def convertChar(char: JsonParser.NonTerminal.StrElem): String =
    char.lift match {
      case JsonParser.Terminal.chars(text, _) => text
      case JsonParser.Terminal.escChar(text, _) =>
        text(1) match {
          case 'n'  => "\n"
          case 't'  => "\t"
          case '\\' => "\\"
          case c    => c.toString
        }
    }

  private def convertString(string: JsonParser.NonTerminal.JsonString): String =
    string._2.toList.map(convertChar).mkString

  def convert(json: JsonParser.NonTerminal.Json): Json =
    json.lift match {
      case array: JsonParser.NonTerminal.JsonArray =>
        array match {
          case JsonParser.NonTerminal.JsonArray._1(_, _)              => Json.JsonArray(Nil)
          case JsonParser.NonTerminal.JsonArray._2(_, children, _, _) => Json.JsonArray(children.toNonEmptyList.toList.map(convert))
        }
      case boolean: JsonParser.NonTerminal.JsonBoolean =>
        boolean.lift match {
          case JsonParser.Terminal.`true`(_)  => Json.JsonBoolean(true)
          case JsonParser.Terminal.`false`(_) => Json.JsonBoolean(false)
        }
      case JsonParser.NonTerminal.JsonInt(value)    => Json.JsonInt(value.text.toInt)
      case JsonParser.NonTerminal.JsonDouble(value) => Json.JsonDouble(BigDecimal(value.text))
      case JsonParser.NonTerminal.JsonNull(_)       => Json.JsonNull
      case jsonObject: JsonParser.NonTerminal.JsonObject =>
        jsonObject match {
          case JsonParser.NonTerminal.JsonObject._1(_, _) => Json.JsonObject(Map.empty)
          case JsonParser.NonTerminal.JsonObject._2(_, children, _, _) =>
            Json.JsonObject(children.toNonEmptyList.toList.map(p => (convertString(p._1), convert(p._3))).toMap)
        }
      case jsonString: JsonParser.NonTerminal.JsonString => Json.JsonString(convertString(jsonString))
    }

}
