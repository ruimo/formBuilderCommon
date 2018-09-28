package com.ruimo.forms.common

import com.ruimo.graphics.twodim.Area
import com.ruimo.scoins.Percent
import play.api.libs.json.{JsArray, JsNumber, JsObject, Json}

// 0 - 254
final class EdgeCropSensivity(val value: Int) extends AnyVal {
  override def toString = "EdgeCropSensivity(" + value + ")"
}

object EdgeCropSensivity {
  def apply(value: Int): EdgeCropSensivity = {
    if (value < 0 || 254 < value)
      throw new IllegalArgumentException("black level(=" + value + ") should be 0-254")
    new EdgeCropSensivity(value)
  }
}

trait EdgeCropCondition {
  def topArea: Option[Area]
  def bottomArea: Option[Area]
  def leftArea: Option[Area]
  def rightArea: Option[Area]

  def asJson: JsObject
}

trait BlackEdgeCropCondition extends EdgeCropCondition {
  def topSensivity: EdgeCropSensivity
  def bottomSensivity: EdgeCropSensivity
  def leftSensivity: EdgeCropSensivity
  def rightSensivity: EdgeCropSensivity
}

trait ColorEdgeCropCondition extends EdgeCropCondition {
  def h: Double
  def s: Percent
  def v: Percent
  def errorPercentage: Percent
}

case class BlackEdgeCropConditionImpl(
  topArea: Option[Area],
  bottomArea: Option[Area],
  leftArea: Option[Area],
  rightArea: Option[Area],
  topSensivity: EdgeCropSensivity,
  bottomSensivity: EdgeCropSensivity,
  rightSensivity: EdgeCropSensivity,
  leftSensivity: EdgeCropSensivity
) extends BlackEdgeCropCondition {
  lazy val asJson: JsObject = {
    def areaToJson(area: Area) = {
      JsArray(
        Seq(
          JsNumber(area.x.value), JsNumber(area.y.value), JsNumber(area.w.value), JsNumber(area.h.value)
        )
      )
    }

    JsObject(
      topArea.map { a => "top" -> areaToJson(a) }.toSeq ++
      bottomArea.map { a => "bottom" -> areaToJson(a) }.toSeq ++
      leftArea.map { a => "left" -> areaToJson(a) }.toSeq ++
      rightArea.map { a => "right" -> areaToJson(a) }.toSeq
    ) ++ Json.obj(
      "topSensivity" -> JsNumber(topSensivity.value),
      "bottomSensivity" -> JsNumber(bottomSensivity.value),
      "leftSensivity" -> JsNumber(leftSensivity.value),
      "rightSensivity" -> JsNumber(rightSensivity.value)
    )
  }
}

