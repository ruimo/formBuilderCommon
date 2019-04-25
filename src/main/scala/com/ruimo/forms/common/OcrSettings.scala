package com.ruimo.forms.common

import com.ruimo.graphics.twodim.Hsv
import com.ruimo.scoins.Percent
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
  val hueValue: Hsv.Hue
  val hueErrorAllowance: Percent
}

object ColorPassFilterSettings {
  def apply(hueValue: Hsv.Hue, hueErrorAllowance: Percent): ColorPassFilterSettings =
    ColorPassFilterSettingsImpl(hueValue, hueErrorAllowance)

  implicit object colorPassFilterSettingsFormat extends Format[ColorPassFilterSettings] {
    override def reads(jv: JsValue): JsResult[ColorPassFilterSettings] = JsSuccess(
      ColorPassFilterSettings(
        Hsv.Hue((jv \ "h").as[Double]),
        Percent((jv \ "hErrorAllowance").as[Double])
      )
    )

    override def writes(f: ColorPassFilterSettings): JsValue = Json.obj(
      "h" -> f.hueValue.value,
      "hErrorAllowance" -> f.hueErrorAllowance.value
    )
  }
}

private case class ColorPassFilterSettingsImpl(
  hueValue: Hsv.Hue, // 0 <= hue < 360
  hueErrorAllowance: Percent // 0 <= error <= 100
) extends ColorPassFilterSettings

trait BinarizationSettings {
  val brightnessThreshold: Int
}

object BinarizationSettings {
  def apply(brightnessThreshold: Int): BinarizationSettings =
    BinarizationSettingsImpl(brightnessThreshold)

  implicit object binarizationSettingsFormat extends Format[BinarizationSettings] {
    override def reads(jv: JsValue): JsResult[BinarizationSettings] = JsSuccess(
      BinarizationSettings(
        (jv \ "brightnessThreshold").as[Int]
      )
    )

    override def writes(f: BinarizationSettings): JsValue = Json.obj(
      "brightnessThreshold" -> f.brightnessThreshold
    )
  }
}

private case class BinarizationSettingsImpl(
  brightnessThreshold: Int // 0 - 255
) extends BinarizationSettings

sealed trait OcrSettings {
  def colorPassFilter: imm.Seq[ColorPassFilterSettings]
  def binarization: Option[BinarizationSettings]
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

trait MonoSpacedSettings {
  val enabled: Boolean
  val hEdgeThresholdPerHeight: Percent
  val vEdgeThresholdPerHeight: Percent
  val acceptableXgap: Percent
  val acceptableYgap: Percent
  val minCharBodyWidthPerHeight: Percent
  val minCharWidthPerHeight: Percent
  val maxCharWidthPerHeight: Percent
}

object MonoSpacedSettings {
  val DefaultHEdgeThresholdPerHeight = Percent(5)
  val DefaultVEdgeThresholdPerHeight = Percent(5)
  val DefaultAcceptableXgap = Percent(15)
  val DefaultAcceptableYgap = Percent(5)
  val DefaultMinCharBodyWidthPerHeight = Percent(15)
  val DefaultMinCharWidthPerHeight = Percent(50)
  val DefaultMaxCharWidthPerHeight = Percent(110)

  def apply(
    enabled: Boolean = false,
    hEdgeThresholdPerHeight: Percent = DefaultHEdgeThresholdPerHeight,
    vEdgeThresholdPerHeight: Percent = DefaultVEdgeThresholdPerHeight,
    acceptableXgap: Percent = DefaultAcceptableXgap,
    acceptableYgap: Percent = DefaultAcceptableYgap,
    minCharBodyWidthPerHeight: Percent = DefaultMinCharBodyWidthPerHeight,
    minCharWidthPerHeight: Percent = DefaultMinCharWidthPerHeight,
    maxCharWidthPerHeight: Percent = DefaultMaxCharWidthPerHeight
  ): MonoSpacedSettings = MonoSpacedSettingsImpl(
    enabled,
    hEdgeThresholdPerHeight, vEdgeThresholdPerHeight,
    acceptableXgap, acceptableYgap,
    minCharBodyWidthPerHeight,
    minCharWidthPerHeight, maxCharWidthPerHeight
  )

  implicit object monoSpacedSettingsFormat extends Format[MonoSpacedSettings] {
    override def reads(jv: JsValue): JsResult[MonoSpacedSettings] = JsSuccess(
      MonoSpacedSettings(
        (jv \ "enabled").as[Boolean],
        Percent((jv \ "hEdgeThresholdPerHeight").as[Double]),
        Percent((jv \ "vEdgeThresholdPerHeight").as[Double]),
        Percent((jv \ "acceptableXgap").as[Double]),
        Percent((jv \ "acceptableYgap").as[Double]),
        Percent((jv \ "minCharBodyWidthPerHeight").as[Double]),
        Percent((jv \ "minCharWidthPerHeight").as[Double]),
        Percent((jv \ "maxCharWidthPerHeight").as[Double])
      )
    )

    override def writes(f: MonoSpacedSettings): JsValue = Json.obj(
      "enabled" -> f.enabled,
      "hEdgeThresholdPerHeight" -> f.hEdgeThresholdPerHeight.value,
      "vEdgeThresholdPerHeight" -> f.vEdgeThresholdPerHeight.value,
      "acceptableXgap" -> f.acceptableXgap.value,
      "acceptableYgap" -> f.acceptableYgap.value,
      "minCharBodyWidthPerHeight" -> f.minCharBodyWidthPerHeight.value,
      "minCharWidthPerHeight" -> f.minCharWidthPerHeight.value,
      "maxCharWidthPerHeight" -> f.maxCharWidthPerHeight.value
    )
  }
}

private case class MonoSpacedSettingsImpl(
  enabled: Boolean,
  hEdgeThresholdPerHeight: Percent, vEdgeThresholdPerHeight: Percent,
  acceptableXgap: Percent, acceptableYgap: Percent,
  minCharBodyWidthPerHeight: Percent,
  minCharWidthPerHeight: Percent, maxCharWidthPerHeight: Percent
) extends MonoSpacedSettings

trait TesseractOcrSettings extends OcrSettings {
  def lang: TesseractLang
  def acceptChars: TesseractAcceptChars
  def monoSpacedSettings: MonoSpacedSettings
}

object TesseractOcrSettings {
  def apply(
    colorPassFilter: imm.Seq[ColorPassFilterSettings],
    binarization: Option[BinarizationSettings],
    lang: TesseractLang,
    acceptChars: TesseractAcceptChars,
    monoSpacedSettings: MonoSpacedSettings = MonoSpacedSettings()
  ): TesseractOcrSettings = TesseractOcrSettingsImpl(
    colorPassFilter, binarization, lang, acceptChars, monoSpacedSettings
  )

  implicit object tesseractOcrSettingsFormat extends Format[TesseractOcrSettings] {
    override def reads(jv: JsValue): JsResult[TesseractOcrSettings] = JsSuccess(
      TesseractOcrSettings(
        (jv \ "colorPassFilter").asOpt[Array[ColorPassFilterSettings]].getOrElse(Array()).toList,
        (jv \ "binarization").asOpt[BinarizationSettings],
        (jv \ "lang").as[TesseractLang],
        (jv \ "acceptChars").as[TesseractAcceptChars],
        (jv \ "monoSpacedSettings").asOpt[MonoSpacedSettings].getOrElse(MonoSpacedSettings())
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
      ),
      "monoSpacedSettings" -> (
        f.monoSpacedSettings match {
          case ms: MonoSpacedSettingsImpl => ms
        }
      )
    ) ++ f.binarization.map(b => Json.obj("binarization" -> b)).getOrElse(Json.obj())
  }
}

trait GoogleOcrSettings extends OcrSettings {
  def lang: GoogleOcrLang
}

object GoogleOcrSettings {
  def apply(
    colorPassFilter: imm.Seq[ColorPassFilterSettings],
    binarization: Option[BinarizationSettings],
    lang: GoogleOcrLang
  ): GoogleOcrSettings = GoogleOcrSettingsImpl(colorPassFilter, binarization, lang)

  implicit object googleOcrSettingsFormat extends Format[GoogleOcrSettings] {
    override def reads(jv: JsValue): JsResult[GoogleOcrSettings] = JsSuccess(
      GoogleOcrSettings(
        (jv \ "colorPassFilter").asOpt[Array[ColorPassFilterSettings]].getOrElse(Array()).toList,
        (jv \ "binarization").asOpt[BinarizationSettings],
        (jv \ "lang").as[GoogleOcrLang]
      )
    )

    override def writes(f: GoogleOcrSettings): JsValue = Json.obj(
      "engine" -> "google",
      "colorPassFilter" -> f.colorPassFilter,
      "lang" -> f.lang
    ) ++ f.binarization.map(b => Json.obj("binarization" -> b)).getOrElse(Json.obj())
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
    binarization: Option[BinarizationSettings],
    useLangModel: Boolean,
    isMultiLine: Boolean,
    acceptChars: TegakiAcceptChars
  ): TegakiOcrSettings = TegakiOcrSettingsImpl(colorPassFilter, binarization, useLangModel, isMultiLine, acceptChars)

  implicit object tegakiOcrSettingsFormat extends Format[TegakiOcrSettings] {
    override def reads(jv: JsValue): JsResult[TegakiOcrSettings] = JsSuccess(
      TegakiOcrSettings(
        (jv \ "colorPassFilter").asOpt[Array[ColorPassFilterSettings]].getOrElse(Array()).toList,
        (jv \ "binarization").asOpt[BinarizationSettings],
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
    ) ++ f.binarization.map(b => Json.obj("binarization" -> b)).getOrElse(Json.obj())
  }
}

private case class TesseractOcrSettingsImpl(
  colorPassFilter: imm.Seq[ColorPassFilterSettings],
  binarization: Option[BinarizationSettings],
  lang: TesseractLang,
  acceptChars: TesseractAcceptChars,
  monoSpacedSettings: MonoSpacedSettings
) extends TesseractOcrSettings

private case class GoogleOcrSettingsImpl(
  colorPassFilter: imm.Seq[ColorPassFilterSettings],
  binarization: Option[BinarizationSettings],
  lang: GoogleOcrLang
) extends GoogleOcrSettings

private case class TegakiOcrSettingsImpl(
  colorPassFilter: imm.Seq[ColorPassFilterSettings],
  binarization: Option[BinarizationSettings],
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
