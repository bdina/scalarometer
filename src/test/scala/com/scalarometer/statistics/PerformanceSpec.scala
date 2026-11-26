package com.scalarometer.statistics

import java.lang.management.ManagementFactory
import java.util.logging.Logger
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PerformanceSpec extends AnyFlatSpec with Matchers {
  private val logger: Logger = Logger.getLogger(classOf[PerformanceSpec].getName)

  private val threadMxBeanOpt: Option[com.sun.management.ThreadMXBean] =
    ManagementFactory.getThreadMXBean match {
      case sunBean: com.sun.management.ThreadMXBean if sunBean.isThreadAllocatedMemorySupported =>
        if (!sunBean.isThreadAllocatedMemoryEnabled) {
          sunBean.setThreadAllocatedMemoryEnabled(true)
        }
        if (sunBean.isThreadAllocatedMemoryEnabled) Some(sunBean) else None
      case _ => None
    }

  private inline def measureMinBytes(warmup: Int, iterations: Int)(inline op: Long): (Long, Long) = {
    var sink = 0L
    var w = 0
    while (w < warmup) {
      sink ^= op
      w += 1
    }

    threadMxBeanOpt match {
      case Some(mx) =>
        var minBytes = Long.MaxValue
        var trial = 0
        while (trial < 3) {
          val before = mx.getCurrentThreadAllocatedBytes
          var i = 0
          while (i < iterations) {
            sink ^= op
            i += 1
          }
          val allocated = mx.getCurrentThreadAllocatedBytes - before
          if (allocated < minBytes) minBytes = allocated
          trial += 1
        }
        (minBytes, sink)
      case None =>
        var i = 0
        while (i < iterations) {
          sink ^= op
          i += 1
        }
        (0L, sink)
    }
  }

  "Counter" should "perform hot-path increments, decrements, reads, and bounded CAS with zero steady-state allocations" in {
    val c = Counter("perf.counter")
    val iterations = 50_000

    val (bytes, _) = measureMinBytes(warmup = 20_000, iterations = iterations) {
      val a = c.increment(2L)
      val b = c.decrement(1L)
      val d = c.decrementByUnless(1L, 0L)
      a ^ b ^ d ^ c.value
    }

    logger.info(s"Counter hot path ($iterations x 4 ops): $bytes bytes allocated")
    bytes shouldBe 0L
    c.value shouldBe 0L
  }

  "Histogram" should "record snapshots across reset windows with zero steady-state allocations and unboxed stats" in {
    val h = Histogram("perf.hist", max_snapshots = 1024)
    var sink = 0L

    // Warm up and grow primitive buffer to max_snapshots
    var cycle = 0
    while (cycle < 10) {
      h.reset()
      var i = 0
      while (i < 1024) {
        sink ^= h.incrementByAndRecordSnapshot((i + 1).toLong)
        i += 1
      }
      cycle += 1
    }

    val (recordBytes, recordSink) = measureMinBytes(warmup = 5, iterations = 10) {
      h.reset()
      var localSink = 0L
      var i = 0
      while (i < 1024) {
        localSink ^= h.incrementByAndRecordSnapshot((i + 1).toLong)
        i += 1
      }
      localSink
    }
    logger.info(s"Histogram steady-state recording (10 x 1024 snapshots, sink=${sink ^ recordSink}): $recordBytes bytes allocated")
    recordBytes shouldBe 0L

    val statsIterations = 200
    val (statsBytes, _) = measureMinBytes(warmup = 50, iterations = statsIterations) {
      java.lang.Double.doubleToRawLongBits(h.mean + h.variance)
    }
    val bytesPerCall = statsBytes / statsIterations
    logger.info(s"Histogram mean+variance on 1024 samples: $bytesPerCall bytes/call")
    // Two primitive long[1024] copies (~16.4 KB) with zero boxed Double loop allocations
    bytesPerCall should be <= 20_480L
    h.mean shouldBe 512.5 +- 1e-9
    h.numberOfSnapshots shouldBe 1024
  }

  "StopWatch" should "perform opaque Marker creation, elapsed checks, reset, and inline timed without timing overhead" in {
    val sw = StopWatch()
    sw.start()
    val iterations = 50_000

    val (markBytes, _) = measureMinBytes(warmup = 20_000, iterations = iterations) {
      val m = sw.mark()
      StopWatch.elapsed(m) ^ m.elapsedNanos ^ m.timestamp
    }
    logger.info(s"StopWatch opaque Marker ($iterations ops): $markBytes bytes allocated")
    markBytes shouldBe 0L

    val (resetBytes, _) = measureMinBytes(warmup = 20_000, iterations = iterations) {
      sw.reset()
      sw.elapsedNanos
    }
    logger.info(s"StopWatch reset ($iterations ops): $resetBytes bytes allocated")
    resetBytes shouldBe 0L

    val (timedBytes, _) = measureMinBytes(warmup = 20_000, iterations = iterations) {
      val (v, ns) = StopWatch.timedNanos {
        42L
      }
      v ^ ns
    }
    val timedBytesPerOp = timedBytes / iterations
    logger.info(s"StopWatch.timedNanos ($iterations ops): $timedBytesPerOp bytes/op")
    timedBytesPerOp should be <= 64L

    var customTick = 1_000_000L
    val customSw = StopWatch(() => customTick)
    customSw.start()
    val (customClockBytes, _) = measureMinBytes(warmup = 20_000, iterations = iterations) {
      customTick += 1_000L
      val m = customSw.mark()
      customSw.elapsedNanos ^ m.timestamp
    }
    logger.info(s"StopWatch custom LongSupplier clock ($iterations ops): $customClockBytes bytes allocated")
    customClockBytes shouldBe 0L
  }

  "Registry" should "perform O(1) queries, empty stats, and no-op unregisters with zero allocations" in {
    val emptyRegistry = Registry()
    val iterations = 50_000

    val (emptyStatsBytes, _) = measureMinBytes(warmup = 20_000, iterations = iterations) {
      emptyRegistry.stats().size.toLong
    }
    logger.info(s"Registry empty stats ($iterations ops): $emptyStatsBytes bytes allocated")
    emptyStatsBytes shouldBe 0L

    val registry = Registry()
    val counters = (0 until 20).map(i => Counter(s"c.$i"))
    val fixed = Counter("fixed.total")
    val absent = Counter("absent.counter")
    registry ++= counters
    registry.registerFixedCounter(fixed)
    counters.foreach(_.increment(1L))
    fixed.increment(10L)

    val (queryBytes, _) = measureMinBytes(warmup = 20_000, iterations = iterations) {
      val s = registry.size.toLong
      val e = if (registry.isEmpty) 1L else 0L
      val c = if (registry.contains("c.10")) 1L else 0L
      val f = if (registry.contains("fixed.total")) 1L else 0L
      val m = if (registry.contains("missing")) 1L else 0L
      s ^ e ^ c ^ f ^ m
    }
    logger.info(s"Registry queries ($iterations x 5 ops): $queryBytes bytes allocated")
    queryBytes shouldBe 0L

    val (noopUnregBytes, _) = measureMinBytes(warmup = 10_000, iterations = iterations) {
      registry.unregisterMetric(absent).size.toLong
    }
    logger.info(s"Registry no-op unregister ($iterations ops): $noopUnregBytes bytes allocated")
    noopUnregBytes shouldBe 0L

    val snap = registry.stats(reset = false)
    snap.size shouldBe 21
    snap("c.10").longValue() shouldBe 1L
    snap("fixed.total").longValue() shouldBe 10L
  }
}
