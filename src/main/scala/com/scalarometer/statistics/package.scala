package com.statistics

trait StatisticsLogging {
  import java.util.logging.Logger
  val logger: Logger = Logger.getLogger(this.getClass.getName)
}

trait CountableMetric {
  def count: Number
  def reset(): Unit
}

trait Metric {
  def name: String
  def data(reset: Boolean): Map[String, Number]
}

trait SnapshotableMetric {
  def snapshots: Vector[Number]
}

trait StatisticsBean {
  def getStatistics(reset: Boolean): Map[String, Number]
}

trait Statistics extends StatisticsLogging {
  import java.lang.management.ManagementFactory
  def initializeJmx(): Unit = {
    if (System.getProperty("com.sun.management.jmxremote") == null) {
      logger.severe("JMX remote is disabled")
    } else {
      val mbs = ManagementFactory.getPlatformMBeanServer()
      try {
        val readerBean = ReaderBean()
        mbs.registerMBean(readerBean, ReaderBean.objectName)
        logger.info("JMX MBean successfully registered")
      } catch {
        case e: Exception => logger.severe(s"Failed to register JMX Agent: ${e.getMessage}")
      }
      val portString = System.getProperty("com.sun.management.jmxremote.port")
      if (portString != null) {
        logger.info(s"JMX running on port: $portString")
      } else {
        logger.finest("JMX port undefined")
      }
    }
  }
}
