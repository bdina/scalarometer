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

  it should "support bulk ++= / --=, deduplicated Iterable ops, empty stats fast path, and duplicate names" in {
    val registry = Registry()
    registry.stats() should be theSameInstanceAs Map.empty[String, Number]
    registry.isEmpty shouldBe true
    registry.size shouldBe 0

    val c1 = Counter("m1")
    val c2 = Counter("m2")
    val c3 = Counter("m3")
    val h1 = Histogram("h1")
    val ac1 = AggregateCounter("ac1")
    val g1 = Gauge.long("g1")(77L)

    c1.increment(3L)
    h1.incrementByAndRecordSnapshot(10L)
    ac1.increment(5L)

    // Bulk add from a single-use Iterator as well as List
    registry ++= Iterator(c1, c2)
    registry ++= List(h1, ac1, g1)
    registry.registerFixedMetric(c3)
    // Register same instance in both buckets to verify deduplicated size/iterator/foreach
    registry.registerFixedMetric(c1)

    registry.size shouldBe 6
    registry.allMetrics.size shouldBe 6
    registry.iterator.toList.size shouldBe 6
    var visited = 0
    registry.foreach(_ => visited += 1)
    visited shouldBe 6

    val snap = registry.stats(reset = false)
    snap("m1").longValue() shouldBe 3L
    snap("h1").longValue() shouldBe 10L
    snap("h1.0.5").doubleValue() shouldBe 10.0
    snap("ac1.avg").doubleValue() shouldBe 5.0
    snap("g1").longValue() shouldBe 77L

    // Duplicate metric names in the same bucket should fall back cleanly on unregister
    val dupA = Counter("dup")
    val dupB = Counter("dup")
    registry += dupA += dupB
    registry.metric("dup") shouldBe defined
    registry.unregisterMetric(dupB)
    registry.metric("dup") shouldBe Some(dupA)
    registry.unregisterMetric(dupA)
    registry.metric("dup") shouldBe None

    // Bulk remove across both resettable and fixed buckets
    registry --= List(c1, c3, g1)
    registry.contains("m1") shouldBe false
    registry.contains("m3") shouldBe false
    registry.contains("g1") shouldBe false
    registry.contains("m2") shouldBe true
    registry.size shouldBe 3

    // Fixed-only fast paths
    registry.clear()
    registry.registerFixedMetric(c3)
    registry.size shouldBe 1
    registry.allMetrics shouldBe Set[Metric](c3)
    registry.iterator.toList shouldBe List[Metric](c3)
    var fixedVisited = 0
    registry.foreach(_ => fixedVisited += 1)
    fixedVisited shouldBe 1
  }

  it should "support AutoCloseable MetricScope via scoped, scopedAll, Using, and withScope" in {
    val registry = Registry()
    val persistent = Counter("service.uptime")
    registry.registerFixedCounter(persistent)

    val streamBytes = Counter("stream.bytes")
    val streamLatency = Histogram("stream.latency")

    val scope = registry.scoped(streamBytes, streamLatency)
    scope.isClosed shouldBe false
    scope.metrics.length shouldBe 2
    registry.contains("stream.bytes") shouldBe true
    registry.contains("stream.latency") shouldBe true
    registry.size shouldBe 3

    scope.close()
    scope.isClosed shouldBe true
    scope.close() // idempotent
    registry.contains("stream.bytes") shouldBe false
    registry.contains("stream.latency") shouldBe false
    registry.contains("service.uptime") shouldBe true
    registry.size shouldBe 1

    val temp = Counter("temp.using")
    val usingResult = scala.util.Using.resource(registry.scopedAll(List(temp))) { s =>
      s.isClosed shouldBe false
      registry.contains("temp.using") shouldBe true
      temp.increment(5L)
    }
    usingResult shouldBe 5L
    registry.contains("temp.using") shouldBe false

    val scopedMetric = Counter("inline.scope")
    val value = registry.withScope(scopedMetric) {
      registry.contains("inline.scope") shouldBe true
      scopedMetric.increment(9L)
    }
    value shouldBe 9L
    registry.contains("inline.scope") shouldBe false

    val failingMetric = Counter("failing.scope")
    intercept[IllegalStateException] {
      registry.withScope(failingMetric) {
        registry.contains("failing.scope") shouldBe true
        throw new IllegalStateException("boom")
      }
    }
    registry.contains("failing.scope") shouldBe false
    registry.contains("service.uptime") shouldBe true

    val emptyScope = registry.scoped()
    emptyScope.metrics.length shouldBe 0
    emptyScope.close()
    emptyScope.isClosed shouldBe true
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
