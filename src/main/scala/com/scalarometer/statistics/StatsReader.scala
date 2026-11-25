package com.scalarometer.statistics

import javax.management.ObjectName

trait ReaderBeanMBean extends StatisticsBean

class ReaderBean(registry: Registry = Registry.global) extends ReaderBeanMBean {
  override def getStatistics(reset: Boolean): Map[String, Number] = registry.stats(reset)
  def objectName: ObjectName = ReaderBean.objectName
}

object ReaderBean {
  val objectName: ObjectName = new ObjectName("com.scalarometer.statistics:type=ReaderBean")
  def apply(registry: Registry = Registry.global): ReaderBean = new ReaderBean(registry)
}
