package com.scalarometer.statistics

import java.lang.management.ManagementFactory
import java.util.logging.{Level, Logger}
import scala.util.control.NonFatal

private object LoggerCache extends ClassValue[Logger] {
  override def computeValue(cls: Class[?]): Logger = Logger.getLogger(cls.getName)
}

trait StatisticsLogging {
  def logger: Logger = LoggerCache.get(this.getClass)

  inline protected def logFine(inline msg: String): Unit = {
    if (logger.isLoggable(Level.FINE)) logger.fine(msg)
  }

  inline protected def logFinest(inline msg: String): Unit = {
    if (logger.isLoggable(Level.FINEST)) logger.finest(msg)
  }

  inline protected def logInfo(inline msg: String): Unit = {
    if (logger.isLoggable(Level.INFO)) logger.info(msg)
  }
}

trait CountableMetric {
  def count: Number
  def reset(): Unit
}

trait Metric {
  def name: String
  def data(reset: Boolean = false): Map[String, Number]

  def appendData(
      builder: scala.collection.mutable.Builder[(String, Number), ?],
      reset: Boolean = false
  ): Unit = {
    val _ = builder ++= data(reset)
  }
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
    if (System.getProperty("com.sun.management.jmxremote") eq null) {
      logger.severe("JMX remote is disabled")
    } else {
      val mbs = ManagementFactory.getPlatformMBeanServer()
      try {
        val bean = readerBean()
        val _ = mbs.registerMBean(bean, bean.objectName)
        logInfo("JMX MBean successfully registered")
      } catch {
        case NonFatal(e) =>
          logger.severe(s"Failed to register JMX Agent: ${e.getMessage}")
      }
      val portString = System.getProperty("com.sun.management.jmxremote.port")
      if (portString ne null) {
        logInfo(s"JMX running on port: $portString")
      } else {
        logFinest("JMX port undefined")
      }
    }
  }

  def unregisterJmx(): Unit = {
    try {
      val mbs = ManagementFactory.getPlatformMBeanServer()
      val objectName = readerBean().objectName
      if (mbs.isRegistered(objectName)) {
        mbs.unregisterMBean(objectName)
        logInfo("JMX MBean successfully unregistered")
      }
    } catch {
      case NonFatal(e) =>
        logger.warning(s"Failed to unregister JMX Agent: ${e.getMessage}")
    }
  }
}
