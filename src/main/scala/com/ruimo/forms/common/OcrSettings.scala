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

trait TegakiAcceptChars {
  def isHiragana: Boolean
  def isKatakan: Boolean
  def isKanji: Boolean
  def isNumber: Boolean
  def isUpperAlpha: Boolean
  def isLowerAlpha: Boolean
  def isSymbol: Boolean
}

object TegakiAcceptChars {
  def apply(
    isHiragana: Boolean,
    isKatakan: Boolean,
    isKanji: Boolean,
    isNumber: Boolean,
    isUpperAlpha: Boolean,
    isLowerAlpha: Boolean,
    isSymbol: Boolean
  ): TegakiAcceptChars = TegakiAcceptCharsImpl(isHiragana, isKatakan, isKanji, isNumber, isUpperAlpha, isLowerAlpha, isSymbol)

  implicit object tegakiAcceptCharsFormat extends Format[TegakiAcceptChars] {
    override def reads(jv: JsValue): JsResult[TegakiAcceptChars] = JsSuccess(
      TegakiAcceptChars(
        (jv \ "isHiragana").as[Boolean],
        (jv \ "isKatakan").as[Boolean],
        (jv \ "isKanji").as[Boolean],
        (jv \ "isNumber").as[Boolean],
        (jv \ "isUpperAlpha").as[Boolean],
        (jv \ "isLowerAlpha").as[Boolean],
        (jv \ "isSymbol").as[Boolean]
      )
    )

    override def writes(f: TegakiAcceptChars): JsValue = Json.obj(
      "isHiragana" -> f.isHiragana,
      "isKatakan" -> f.isKatakan,
      "isKanji" -> f.isKanji,
      "isNumber" -> f.isNumber,
      "isUpperAlpha" -> f.isUpperAlpha,
      "isLowerAlpha" -> f.isLowerAlpha,
      "isSymbol" -> f.isSymbol
    )
  }
}

private case class TegakiAcceptCharsImpl(
  isHiragana: Boolean,
  isKatakan: Boolean,
  isKanji: Boolean,
  isNumber: Boolean,
  isUpperAlpha: Boolean,
  isLowerAlpha: Boolean,
  isSymbol: Boolean
) extends TegakiAcceptChars

trait ColorPassFilterSettings {
  val hueValue: Double // 0 <= hue < 360
  val hueErrorAllowance: Double // 0 <= error <= 100
}

object ColorPassFilterSettings {
  def apply(hueValue: Double, hueErrorAllowance: Double): ColorPassFilterSettings =
    ColorPassFilterSettingsImpl(hueValue, hueErrorAllowance)

  implicit object colorPassFilterSettingsFormat extends Format[ColorPassFilterSettings] {
    override def reads(jv: JsValue): JsResult[ColorPassFilterSettings] = JsSuccess(
      ColorPassFilterSettings(
        (jv \ "h").as[Double],
        (jv \ "hErrorAllowance").as[Double]
      )
    )

    override def writes(f: ColorPassFilterSettings): JsValue = Json.obj(
      "h" -> f.hueValue,
      "hErrorAllowance" -> f.hueErrorAllowance
    )
  }
}

private case class ColorPassFilterSettingsImpl(
  hueValue: Double, // 0 <= hue < 360
  hueErrorAllowance: Double // 0 <= error <= 100
) extends ColorPassFilterSettings

sealed trait OcrSettings {
  def colorPassFilter: imm.Seq[ColorPassFilterSettings]
}

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

object GoogleOcrLang {
  implicit object googleOcrLangFormat extends Format[GoogleOcrLang] {
    override def reads(jv: JsValue): JsResult[GoogleOcrLang] = {
      val code = jv.as[String]
      JsSuccess(
        if (code == GoogleOcrLangEn.code) GoogleOcrLangEn else GoogleOcrLangJa
      )
    }

    override def writes(d: GoogleOcrLang): JsValue = JsString(d.code)
  }
}

trait TesseractOcrSettings extends OcrSettings {
  def lang: TesseractLang
  def acceptChars: TesseractAcceptChars
}

object TesseractOcrSettings {
  def apply(
    colorPassFilter: imm.Seq[ColorPassFilterSettings],
    lang: TesseractLang,
    acceptChars: TesseractAcceptChars
  ): TesseractOcrSettings = TesseractOcrSettingsImpl(colorPassFilter, lang, acceptChars)

  implicit object tesseractOcrSettingsFormat extends Format[TesseractOcrSettings] {
    override def reads(jv: JsValue): JsResult[TesseractOcrSettings] = JsSuccess(
      TesseractOcrSettings(
        (jv \ "colorPassFilter").as[Array[ColorPassFilterSettings]].toList,
        (jv \ "lang").as[TesseractLang],
        (jv \ "acceptChars").as[TesseractAcceptChars]
      )
    )

    override def writes(f: TesseractOcrSettings): JsValue = Json.obj(
      "engine" -> "tesseract",
      "colorPassFilter" -> f.colorPassFilter,
      "lang" -> f.lang,
      "acceptChars" -> (
        f.acceptChars match {
          case ta: TesseractAcceptCharsImpl => ta
        }
      )
    )
  }
}

trait GoogleOcrSettings extends OcrSettings {
  def lang: GoogleOcrLang
}

object GoogleOcrSettings {
  def apply(
    colorPassFilter: imm.Seq[ColorPassFilterSettings],
    lang: GoogleOcrLang
  ): GoogleOcrSettings = GoogleOcrSettingsImpl(colorPassFilter, lang)

  implicit object googleOcrSettingsFormat extends Format[GoogleOcrSettings] {
    override def reads(jv: JsValue): JsResult[GoogleOcrSettings] = JsSuccess(
      GoogleOcrSettings(
        (jv \ "colorPassFilter").as[Array[ColorPassFilterSettings]].toList,
        (jv \ "lang").as[GoogleOcrLang]
      )
    )

    override def writes(f: GoogleOcrSettings): JsValue = Json.obj(
      "engine" -> "google",
      "colorPassFilter" -> f.colorPassFilter,
      "lang" -> f.lang
    )
  }
}

trait TegakiOcrSettings extends OcrSettings {
  def useLangModel: Boolean
  def isMultiLine: Boolean
  def acceptChars: TegakiAcceptChars
}

object TegakiOcrSettings {
  def apply(
    colorPassFilter: imm.Seq[ColorPassFilterSettings],
    useLangModel: Boolean,
    isMultiLine: Boolean,
    acceptChars: TegakiAcceptChars
  ): TegakiOcrSettings = TegakiOcrSettingsImpl(colorPassFilter, useLangModel, isMultiLine, acceptChars)

  implicit object tegakiOcrSettingsFormat extends Format[TegakiOcrSettings] {
    override def reads(jv: JsValue): JsResult[TegakiOcrSettings] = JsSuccess(
      TegakiOcrSettings(
        (jv \ "colorPassFilter").as[Array[ColorPassFilterSettings]].toList,
        (jv \ "useLangModel").as[Boolean],
        (jv \ "isMultiLine").as[Boolean],
        (jv \ "acceptChars").as[TegakiAcceptChars]
      )
    )

    override def writes(f: TegakiOcrSettings): JsValue = Json.obj(
      "engine" -> "tegaki",
      "colorPassFilter" -> f.colorPassFilter,
      "useLangModel" -> f.useLangModel,
      "isMultiLine" -> f.isMultiLine,
      "acceptChars" -> (
        f.acceptChars match {
          case ta: TegakiAcceptCharsImpl => ta
        }
      )
    )
  }
}

private case class TesseractOcrSettingsImpl(
  colorPassFilter: imm.Seq[ColorPassFilterSettings],
  lang: TesseractLang,
  acceptChars: TesseractAcceptChars
) extends TesseractOcrSettings

private case class GoogleOcrSettingsImpl(
  colorPassFilter: imm.Seq[ColorPassFilterSettings],
  lang: GoogleOcrLang
) extends GoogleOcrSettings

private case class TegakiOcrSettingsImpl(
  colorPassFilter: imm.Seq[ColorPassFilterSettings],
  useLangModel: Boolean,
  isMultiLine: Boolean,
  acceptChars: TegakiAcceptChars
) extends TegakiOcrSettings

object OcrSettings {
  implicit object ocrSettingsFormat extends Format[OcrSettings] {
    override def reads(jv: JsValue): JsResult[OcrSettings] = {
      (jv \ "engine").as[String] match {
        case "tesseract" => JsSuccess(jv.as[TesseractOcrSettings])
        case "google" => JsSuccess(jv.as[GoogleOcrSettings])
        case "tegaki" => JsSuccess(jv.as[TegakiOcrSettings])
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

      case teos: TegakiOcrSettings => Json.obj(
        "useLangModel" -> teos.useLangModel,
        "isMultiLine" -> teos.isMultiLine,
        "isHiragana" -> teos.acceptChars.isHiragana,
        "isKatakan" -> teos.acceptChars.isKatakan,
        "isKanji" -> teos.acceptChars.isKanji,
        "isNumber" -> teos.acceptChars.isNumber,
        "isUpperAlpha" -> teos.acceptChars.isUpperAlpha,
        "isLowerAlpha" -> teos.acceptChars.isLowerAlpha,
        "isSymbol" -> teos.acceptChars.isSymbol
      )

      case gos: GoogleOcrSettings => Json.obj(
        "lang" -> gos.lang
      )
    }
  }
}
