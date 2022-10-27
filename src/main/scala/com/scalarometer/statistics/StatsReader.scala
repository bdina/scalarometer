package com.statistics

class ReaderBean(registry: Registry = Registry.global) extends StatisticsBean {
  def getStatistics(reset: Boolean): Map[String,Number] = registry.stats(reset)
  def objectName = ReaderBean.objectName
}
object ReaderBean {
  import javax.management.ObjectName
  val objectName = new ObjectName("com.statistics:type=ReaderBean")
  def apply() = new ReaderBean()
}
