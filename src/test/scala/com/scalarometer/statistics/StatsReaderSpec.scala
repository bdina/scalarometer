package com.scalarometer.statistics

import java.lang.management.ManagementFactory
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StatsReaderSpec extends AnyFlatSpec with Matchers {

  "ReaderBean" should "read statistics from its bound Registry instance" in {
    val registry = Registry()
    val counter = Counter("custom.hits")
    counter.increment(15L)
    registry.registerCounter(counter)

    val bean = ReaderBean(registry)
    bean.objectName shouldBe ReaderBean.objectName
    bean.getStatistics(reset = false)("custom.hits").longValue() shouldBe 15L
    bean.getStatistics(reset = true)("custom.hits").longValue() shouldBe 15L
    bean.getStatistics(reset = false)("custom.hits").longValue() shouldBe 0L
  }

  it should "be a compliant JMX Standard MBean registered on the platform MBean server" in {
    val registry = Registry()
    val counter = Counter("jmx.requests")
    counter.increment(7L)
    registry.registerCounter(counter)

    val mbs = ManagementFactory.getPlatformMBeanServer()
    val propKey = "com.sun.management.jmxremote"
    val previousProp = System.getProperty(propKey)

    try {
      registry.unregisterJmx()

      System.clearProperty(propKey)
      registry.initializeJmx()
      mbs.isRegistered(ReaderBean.objectName) shouldBe false

      System.setProperty(propKey, "true")
      registry.initializeJmx()
      mbs.isRegistered(ReaderBean.objectName) shouldBe true

      val stats = mbs
        .invoke(
          ReaderBean.objectName,
          "getStatistics",
          Array[Object](java.lang.Boolean.FALSE),
          Array[String]("boolean")
        )
        .asInstanceOf[Map[String, Number]]

      stats("jmx.requests").longValue() shouldBe 7L
    } finally {
      registry.unregisterJmx()
      if (previousProp == null) System.clearProperty(propKey)
      else System.setProperty(propKey, previousProp)
    }

    mbs.isRegistered(ReaderBean.objectName) shouldBe false
  }
}
