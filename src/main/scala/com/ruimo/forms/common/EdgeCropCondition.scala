package com.ruimo.forms.common

import com.ruimo.graphics.twodim.Area
import com.ruimo.scoins.Percent
import play.api.libs.json.JsObject

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
