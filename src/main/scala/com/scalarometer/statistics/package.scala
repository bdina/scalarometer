package com.scalarometer.statistics

import java.lang.management.ManagementFactory
import java.util.logging.Logger
import scala.util.control.NonFatal

trait StatisticsLogging {
  @transient lazy val logger: Logger = Logger.getLogger(this.getClass.getName)
}

trait CountableMetric {
  def count: Number
  def reset(): Unit
}

trait Metric {
  def name: String
  def data(reset: Boolean = false): Map[String, Number]
}

trait SnapshotableMetric {
  def snapshots: Vector[Number]
}

trait StatisticsBean {
  def getStatistics(reset: Boolean): Map[String, Number]
}

trait Statistics extends StatisticsLogging {
  protected def readerBean(): ReaderBean = ReaderBean()

  def initializeJmx(): Unit = {
    if (System.getProperty("com.sun.management.jmxremote") == null) {
      logger.severe("JMX remote is disabled")
    } else {
      val mbs = ManagementFactory.getPlatformMBeanServer()
      try {
        val bean = readerBean()
        val _ = mbs.registerMBean(bean, bean.objectName)
        logger.info("JMX MBean successfully registered")
      } catch {
        case NonFatal(e) =>
          logger.severe(s"Failed to register JMX Agent: ${e.getMessage}")
      }
      Option(System.getProperty("com.sun.management.jmxremote.port")) match {
        case Some(portString) =>
          logger.info(s"JMX running on port: $portString")
        case None =>
          logger.finest("JMX port undefined")
      }
    }
  }

  def unregisterJmx(): Unit = {
    try {
      val mbs = ManagementFactory.getPlatformMBeanServer()
      val objectName = readerBean().objectName
      if (mbs.isRegistered(objectName)) {
        mbs.unregisterMBean(objectName)
        logger.info("JMX MBean successfully unregistered")
      }
    } catch {
      case NonFatal(e) =>
        logger.warning(s"Failed to unregister JMX Agent: ${e.getMessage}")
    }
  }
}
