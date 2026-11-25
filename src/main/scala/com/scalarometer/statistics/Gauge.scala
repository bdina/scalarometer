package com.scalarometer.statistics

import java.io.Serializable

@SerialVersionUID(20200724110900L)
class Gauge(val name: String, f: (String, Boolean) => Map[String, Number])
    extends Metric
    with Serializable
    with StatisticsLogging {

  override def data(reset: Boolean = false): Map[String, Number] = f(name, reset)

  override def toString(): String = s"Gauge($name)"
}

object Gauge {
  def apply(name: String, f: (String, Boolean) => Map[String, Number]): Gauge =
    new Gauge(name, f)

  def single(name: String)(f: => Number): Gauge =
    new Gauge(name, (n, _) => Map[String, Number](n -> f))
}
