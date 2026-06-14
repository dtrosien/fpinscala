package fpinscala.exercises.parsing

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*
    val spaces = char(' ').many.slice
    val numbers = regex("[0-9]".r).many.slice
    val boolean = string("true") | string("false").slice
    val empty = string("null").slice
    val objOpen = char('{')
    val objClose = char('}')
    val define = char(':')
    val key = regex("".r).many.slice
    val str = regex("".r).many.slice
    val arrayOpen = char('[')
    val arrayClose = char(']')
    val separator = char(',')
    // val arr = arrayOpen ** (((obj | str | numbers | boolean | empty) ** separator).many | (obj | str | numbers | boolean | empty)) ** arrayClose
    // val obj: Parser[Json] = objOpen ** spaces ** key ** define ** (obj | str | numbers | boolean | empty)  ** objclose
    ???
