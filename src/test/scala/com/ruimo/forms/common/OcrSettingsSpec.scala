package com.ruimo.forms.common

import org.specs2.mutable._
import play.api.libs.json.Json

import scala.collection.{immutable => imm}

class OcrSettingsSpec extends Specification {
  "OCR settings" should {
    "can serialize tesseract settings to json." in {
      val s = TesseractOcrSettings(
        imm.Seq(),
        TesseractLangJa,
        TesseractAcceptChars(
          imm.Set(Tesseract.OcrDigit, Tesseract.OcrComma),
          "custom"
        )
      )

      val json = Json.obj("settings" -> s)

      val ds = (json \ "settings").as[TesseractOcrSettings]
      ds === s
    }

    "can serialize google ocr settings to json." in {
      val s = GoogleOcrSettings(
        imm.Seq(),
        GoogleOcrLangJa
      )

      val json = Json.obj("settings" -> s)

      val ds = (json \ "settings").as[GoogleOcrSettings]
      ds === s
    }
  }
}
