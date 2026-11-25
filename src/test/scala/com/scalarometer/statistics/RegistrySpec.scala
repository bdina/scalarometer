package com.scalarometer.statistics

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RegistrySpec extends AnyFlatSpec with Matchers {

  "Registry" should "register, lookup, and unregister regular and fixed metrics" in {
    val registry = Registry()
    val c1 = Counter("req.count")
    val c2 = Counter("uptime.ticks")
    val g1 = Gauge.single("mem.used")(java.lang.Long.valueOf(512L))

    registry.registerCounter(c1) should contain(c1)
    registry.registerFixedCounter(c2) should contain(c2)
    registry.registerMetric(g1) should contain(g1)

    registry.metric("req.count") shouldBe Some(c1)
    registry.metric("mem.used") shouldBe Some(g1)
    registry.metric("uptime.ticks") shouldBe None
    registry.fixedMetric("uptime.ticks") shouldBe Some(c2)
    registry.get("uptime.ticks") shouldBe Some(c2)
    registry.contains("req.count") shouldBe true
    registry.contains("missing") shouldBe false

    registry.unregisterCounter(c1) should not contain c1
    registry.unregisterMetric(g1) shouldBe empty
    registry.unregisterFixedMetric(c2) shouldBe empty
    registry.isEmpty shouldBe true
  }

  it should "reset regular metrics on stats(reset = true) while preserving fixed metrics" in {
    val registry = Registry()
    val resettable = Counter("window.events")
    val fixed = Counter("total.events")
    resettable.increment(10L)
    fixed.increment(100L)

    registry.registerCounter(resettable)
    registry.registerFixedCounter(fixed)

    val first = registry.stats(reset = false)
    first("window.events").longValue() shouldBe 10L
    first("total.events").longValue() shouldBe 100L

    val second = registry.stats(reset = true)
    second("window.events").longValue() shouldBe 10L
    second("total.events").longValue() shouldBe 100L

    val third = registry.stats(reset = false)
    third("window.events").longValue() shouldBe 0L
    third("total.events").longValue() shouldBe 100L
  }

  it should "integrate with Scala collections via Iterable[Metric] and += / -= operators" in {
    val registry = Registry()
    val c1 = Counter("alpha")
    val c2 = AggregateCounter("beta")
    val c3 = Histogram("gamma")

    registry += c1 += c2
    registry.registerFixedMetric(c3)

    registry.size shouldBe 3
    registry.nonEmpty shouldBe true
    registry.map(_.name).toSet shouldBe Set("alpha", "beta", "gamma")
    registry.filter(_.name.startsWith("a")).map(_.name).toList shouldBe List("alpha")

    registry -= c1 -= c3
    registry.map(_.name).toSet shouldBe Set("beta")

    registry.clear()
    registry.isEmpty shouldBe true
  }

  it should "support concurrent metric registration and stats snapshots" in {
    val registry = Registry()
    val threads = 6
    val metricsPerThread = 50
    val pool = Executors.newFixedThreadPool(threads)
    val startLatch = new CountDownLatch(1)
    val doneLatch = new CountDownLatch(threads)

    try {
      for (t <- 0 until threads) {
        pool.submit(new Runnable {
          override def run(): Unit = {
            startLatch.await()
            var i = 0
            while (i < metricsPerThread) {
              val c = Counter(s"t$t.m$i")
              c.increment(1L)
              registry.registerCounter(c)
              if (i % 10 == 0) {
                val _ = registry.stats(reset = false)
              }
              i += 1
            }
            doneLatch.countDown()
          }
        })
      }
      startLatch.countDown()
      doneLatch.await(10, TimeUnit.SECONDS) shouldBe true

      registry.size shouldBe (threads * metricsPerThread)
      registry.stats().size shouldBe (threads * metricsPerThread)
    } finally {
      pool.shutdownNow()
    }
  }
}
