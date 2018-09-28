package com.ruimo.forms.common

import com.ruimo.graphics.twodim.Area
import com.ruimo.scoins.Percent
import play.api.libs.json._

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
  def edge: Edge

  def asJson: JsObject
}

object EdgeCropCondition {
  def areaToJson(area: Area) = {
    JsArray(
      Seq(
        JsNumber(area.x.value), JsNumber(area.y.value), JsNumber(area.w.value), JsNumber(area.h.value)
      )
    )
  }
}

sealed trait Edge

case class BlackEdge(
  topSensivity: EdgeCropSensivity,
  bottomSensivity: EdgeCropSensivity,
  leftSensivity: EdgeCropSensivity,
  rightSensivity: EdgeCropSensivity
) extends Edge

case class ColorEdge(
  h: Double,
  s: Double,
  v: Double,
  errorPercentage: Percent
) extends Edge

trait BlackEdgeCropCondition extends EdgeCropCondition {
  def edge: BlackEdge
}

trait ColorEdgeCropCondition extends EdgeCropCondition {
  def edge: ColorEdge
}

case class BlackEdgeCropConditionImpl(
  topArea: Option[Area],
  bottomArea: Option[Area],
  leftArea: Option[Area],
  rightArea: Option[Area],
  edge: BlackEdge
) extends BlackEdgeCropCondition {
  lazy val asJson: JsObject = {
    JsObject(
      Seq("cropType" -> JsString("blackEdgeCrop")) ++
      topArea.map { a => "top" -> EdgeCropCondition.areaToJson(a) } ++
      bottomArea.map { a => "bottom" -> EdgeCropCondition.areaToJson(a) } ++
      leftArea.map { a => "left" -> EdgeCropCondition.areaToJson(a) } ++
      rightArea.map { a => "right" -> EdgeCropCondition.areaToJson(a) }
    ) ++ Json.obj(
      "topSensivity" -> JsNumber(edge.topSensivity.value),
      "bottomSensivity" -> JsNumber(edge.bottomSensivity.value),
      "leftSensivity" -> JsNumber(edge.leftSensivity.value),
      "rightSensivity" -> JsNumber(edge.rightSensivity.value)
    )
  }
}

case class ColorEdgeCropConditionImpl(
  topArea: Option[Area],
  bottomArea: Option[Area],
  leftArea: Option[Area],
  rightArea: Option[Area],
  edge: ColorEdge
) extends ColorEdgeCropCondition {
  lazy val asJson: JsObject = {
    JsObject(
      Seq("cropType" -> JsString("colorEdgeCrop")) ++
      topArea.map { a => "top" -> EdgeCropCondition.areaToJson(a) } ++
      bottomArea.map { a => "bottom" -> EdgeCropCondition.areaToJson(a) } ++
      leftArea.map { a => "left" -> EdgeCropCondition.areaToJson(a) } ++
      rightArea.map { a => "right" -> EdgeCropCondition.areaToJson(a) }
    ) ++ Json.obj(
      "h" -> JsNumber(edge.h),
      "s" -> JsNumber(edge.s),
      "v" -> JsNumber(edge.v),
      "errorPercentage" -> JsNumber(edge.errorPercentage.value)
    )
  }
}


