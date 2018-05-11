package com.ruimo.forms.common

import scala.collection.{immutable => imm}

object Tesseract {
  sealed trait OcrChars {
    val code: String
    val chars: String
  }
  object OcrChars {
    def tryParse(s: String): Option[OcrChars] = mapFromString.get(s)
  }
  case object OcrDigit extends OcrChars {
    val code = "0-9"
    val chars = "0123456789"
  }
  case object OcrUpperAlphabet extends OcrChars {
    val code = "A-Z"
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  }
  case object OcrLowerALphabet extends OcrChars {
    val code = "a-z"
    val chars = "abcdefghijklmnopqrstuvwxyz"
  }
  case object OcrComma extends OcrChars {
    val code = ","
    val chars = ","
  }
  case object OcrPeriod extends OcrChars {
    val code = "."
    val chars = "."
  }
  case object OcrMinus extends OcrChars {
    val code = "-"
    val chars = "-"
  }
  case object OcrPlus extends OcrChars {
    val code = "+"
    val chars = "+"
  }
  case object OcrSlash extends OcrChars {
    val code = "/"
    val chars = "/"
  }
  case object OcrSharp extends OcrChars {
    val code = "#"
    val chars = "#"
  }
  case object OcrDoller extends OcrChars {
    val code = "$"
    val chars = "$"
  }
  case object OcrBrace extends OcrChars {
    val code = "()"
    val chars = "()"
  }
  case object OcrPercent extends OcrChars {
    val code = "%"
    val chars = "%"
  }
  case object OcrColon extends OcrChars {
    val code = ":"
    val chars = ":"
  }
  case object OcrSemiColon extends OcrChars {
    val code = ";"
    val chars = ";"
  }
  case object OcrEqual extends OcrChars {
    val code = "="
    val chars = "="
  }
  case object OcrAsterisc extends OcrChars {
    val code = "*"
    val chars = "*"
  }

  private[this] val mapFromString: imm.Map[String, OcrChars] = imm.HashMap(
    "0-9" -> OcrDigit,
    "A-Z" -> OcrUpperAlphabet,
    "a-z" -> OcrLowerALphabet,
    "," -> OcrComma,
    "." -> OcrPeriod,
    "-" -> OcrMinus,
    "+" -> OcrPlus,
    "/" -> OcrSlash,
    "#" -> OcrSharp,
    "$" -> OcrDoller,
    "()" -> OcrBrace,
    "%" -> OcrPercent,
    ":" -> OcrColon,
    ";" -> OcrSemiColon,
    "=" -> OcrEqual,
    "*" -> OcrAsterisc
  )
}
