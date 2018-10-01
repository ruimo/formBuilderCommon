package com.ruimo.forms.common

import com.ruimo.graphics.twodim.Area
import com.ruimo.scoins.Percent
import play.api.libs.json._

// 0 - 254
final class EdgeCropSensitivity(val value: Int) extends AnyVal {
  override def toString = "EdgeCropSensitivity(" + value + ")"
}

object EdgeCropSensitivity {
  def apply(value: Int): EdgeCropSensitivity = {
    if (value < 0 || 254 < value)
      throw new IllegalArgumentException("black level(=" + value + ") should be 0-254")
    new EdgeCropSensitivity(value)
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

  def jsonToArea(js: JsLookupResult): Area = {
    val v = js.as[Array[Double]]
    Area(
      Percent(v(0)), Percent(v(1)), Percent(v(2)), Percent(v(3))
    )
  }

  def parse(js: JsValue): EdgeCropCondition = {
    (js \ "cropType").asOpt[String].getOrElse("blackEdgeCrop") match {
      case "blackEdgeCrop" =>
        BlackEdgeCropConditionImpl(
          Some(jsonToArea(js \ "top")),
          Some(jsonToArea(js \ "bottom")),
          Some(jsonToArea(js \ "left")),
          Some(jsonToArea(js \ "right")),
          BlackEdge(
            EdgeCropSensitivity((js \ "topSensitivity").asOpt[Int].getOrElse(254)),
            EdgeCropSensitivity((js \ "bottomSensitivity").asOpt[Int].getOrElse(254)),
            EdgeCropSensitivity((js \ "leftSensitivity").asOpt[Int].getOrElse(254)),
            EdgeCropSensitivity((js \ "rightSensitivity").asOpt[Int].getOrElse(254))
          )
        )
      case "colorEdgeCrop" =>
        ColorEdgeCropConditionImpl(
          Some(jsonToArea(js \ "top")),
          Some(jsonToArea(js \ "bottom")),
          Some(jsonToArea(js \ "left")),
          Some(jsonToArea(js \ "right")),
          ColorEdge(
            (js \ "h").as[Double],
            (js \ "s").as[Double],
            (js \ "v").as[Double],
            Percent((js \ "errorPercentage").as[Double]),
            Percent((js \ "threshold").as[Double])
          )
        )
    }
  }
}

sealed trait Edge

case class BlackEdge(
  topSensitivity: EdgeCropSensitivity,
  bottomSensitivity: EdgeCropSensitivity,
  leftSensitivity: EdgeCropSensitivity,
  rightSensitivity: EdgeCropSensitivity
) extends Edge

case class ColorEdge(
  h: Double,
  s: Double,
  v: Double,
  errorPercentage: Percent,
  threshold: Percent
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
      "topSensitivity" -> JsNumber(edge.topSensitivity.value),
      "bottomSensitivity" -> JsNumber(edge.bottomSensitivity.value),
      "leftSensitivity" -> JsNumber(edge.leftSensitivity.value),
      "rightSensitivity" -> JsNumber(edge.rightSensitivity.value)
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
      "errorPercentage" -> JsNumber(edge.errorPercentage.value),
      "threshold" -> JsNumber(edge.threshold.value)
    )
  }
}


