package com.ruimo.forms.common

import play.api.libs.json._

import scala.collection.{immutable => imm}

trait TesseractAcceptChars {
  def chars: imm.Set[Tesseract.OcrChars]
  def custom: String
  def union: String
}

object TesseractAcceptChars {
  def apply(
    chars: imm.Set[Tesseract.OcrChars],
    custom: String
  ): TesseractAcceptChars = TesseractAcceptCharsImpl(chars, custom)

  implicit object tesseractAcceptCharsFormat extends Format[TesseractAcceptChars] {
    override def reads(jv: JsValue): JsResult[TesseractAcceptChars] = JsSuccess(
      TesseractAcceptChars(
        ((jv \ "chars").as[Seq[String]]).flatMap { s =>
          Tesseract.OcrChars.tryParse(s)
        }.toSet,
        (jv \ "custom").as[String]
      )
    )

    override def writes(f: TesseractAcceptChars): JsValue = Json.obj(
      "chars" -> f.chars.map(_.code),
      "custom" -> f.custom
    )
  }
}

private case class TesseractAcceptCharsImpl(
  chars: imm.Set[Tesseract.OcrChars],
  custom: String
) extends TesseractAcceptChars {
  val union: String = chars.map(_.chars).foldLeft(new StringBuilder)(_.append(_)).append(custom).toString.distinct
}

sealed trait OcrSettings

sealed trait TesseractLang {
  val code: String
  val tesseractLangCode: String
}

case object TesseractLangJa extends TesseractLang {
  val code: String = "ja"
  val tesseractLangCode = "jpn"
  override def toString = "日本語"
}

case object TesseractLangEn extends TesseractLang {
  val code: String = "en"
  val tesseractLangCode = "eng"
  override def toString = "英語"
}

sealed trait GoogleOcrLang {
  val code: String
}

case object GoogleOcrLangJa extends GoogleOcrLang {
  val code: String = "ja"
  override def toString = "日本語"
}

case object GoogleOcrLangEn extends GoogleOcrLang {
  val code: String = "en"
  override def toString = "英語"
}

object TesseractLang {
  implicit object tesseractLangFormat extends Format[TesseractLang] {
    override def reads(jv: JsValue): JsResult[TesseractLang] = {
      val code = jv.as[String]
      JsSuccess(
        if (code == TesseractLangEn.code) TesseractLangEn else TesseractLangJa
      )
    }

    override def writes(d: TesseractLang): JsValue = JsString(d.code)
  }
}

trait TesseractOcrSettings extends OcrSettings {
  def lang: TesseractLang
  def acceptChars: TesseractAcceptChars
}

object TesseractOcrSettings {
  def apply(
    lang: TesseractLang,
    acceptChars: TesseractAcceptChars
  ): TesseractOcrSettings = TesseractOcrSettingsImpl(lang, acceptChars)

  implicit object tesseractOcrSettingsFormat extends Format[TesseractOcrSettings] {
    override def reads(jv: JsValue): JsResult[TesseractOcrSettings] = JsSuccess(
      TesseractOcrSettings(
        (jv \ "lang").as[TesseractLang],
        (jv \ "acceptChars").as[TesseractAcceptChars]
      )
    )

    override def writes(f: TesseractOcrSettings): JsValue = Json.obj(
      "engine" -> "tesseract",
      "lang" -> f.lang,
      "acceptChars" -> (
        f.acceptChars match {
          case ta: TesseractAcceptCharsImpl => ta
        }
      )
    )
  }
}

private case class TesseractOcrSettingsImpl(
  lang: TesseractLang,
  acceptChars: TesseractAcceptChars
) extends TesseractOcrSettings

object OcrSettings {
  implicit object ocrSettingsFormat extends Format[OcrSettings] {
    override def reads(jv: JsValue): JsResult[OcrSettings] = {
      (jv \ "engine").as[String] match {
        case "tesseract" => JsSuccess(jv.as[TesseractOcrSettings])
        case that: String => JsError(JsPath \ "engine", "Unknown OCR engine name '" + that + "'")
      }
    }

    override def writes(os: OcrSettings): JsValue = os match {
      case tos: TesseractOcrSettings => Json.obj(
        "lang" -> tos.lang,
        "acceptChars" -> (
          tos.acceptChars match {
            case ta: TesseractAcceptCharsImpl => ta
          }
        )
      )
    }
  }
}
