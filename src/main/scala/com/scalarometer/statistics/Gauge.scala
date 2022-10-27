package com.statistics

class Gauge(val name: String, f: Function2[String,Boolean,Map[String,Number]]) extends Metric with Serializable with StatisticsLogging {
  val serialVersionUID = 20200724110900L

  def data(reset: Boolean): Map[String, Number] = f.apply(name, reset)
}
object Gauge {
  def apply(name: String, f: Function2[String,Boolean,Map[String,Number]]) = new Gauge(name,f)
}
