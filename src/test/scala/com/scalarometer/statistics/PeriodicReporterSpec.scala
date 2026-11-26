package com.scalarometer.statistics

import java.time.Duration
import java.util.concurrent.{CopyOnWriteArrayList, CountDownLatch, Executors, TimeUnit}
import java.util.logging.{Handler, Level, LogRecord, Logger}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class PeriodicReporterSpec extends AnyFlatSpec with Matchers {

  "PeriodicReporter" should "render snapshots via reportOnce, respect skipWhenEmpty, resettable/fixed buckets, and flushOnClose" in {
    val registry = Registry()
    val emitted = new CopyOnWriteArrayList[String]()
    val sharedExec = Executors.newSingleThreadScheduledExecutor()

    try {
      val reporter = PeriodicReporter.start(
        registry = registry,
        interval = Duration.ofHours(1),
        format = MetricsFormat.Logfmt,
        reset = true,
        prefix = "[service:metrics] ",
        skipWhenEmpty = true,
        flushOnClose = true,
        executor = Some(sharedExec)
      )(msg => emitted.add(msg))

      // Empty registry skipped
      reporter.reportOnce() shouldBe None
      emitted.size() shouldBe 0

      val req = Counter("http.requests")
      val uptime = Counter("service.uptime")
      req.increment(4L)
      uptime.increment(100L)
      registry += req
      registry.registerFixedCounter(uptime)

      val first = reporter.reportOnce()
      first shouldBe Some("[service:metrics] http.requests=4 service.uptime=100")
      emitted.size() shouldBe 1
      req.value shouldBe 0L
      uptime.value shouldBe 100L

      req.increment(2L)
      reporter.close()
      reporter.isClosed shouldBe true
      reporter.close() // idempotent: no second flush

      emitted.size() shouldBe 2
      emitted.get(1) shouldBe "[service:metrics] http.requests=2 service.uptime=100"
      // Shared executor must NOT be shut down by reporter.close()
      sharedExec.isShutdown shouldBe false
    } finally {
      sharedExec.shutdownNow()
    }
  }

  it should "run scheduled ticks on a daemon thread and isolate NonFatal sink failures" in {
    val registry = Registry()
    val c = Counter("ticks")
    c.increment(1L)
    registry += c

    val tickLatch = new CountDownLatch(3)
    val threadIsDaemon = new java.util.concurrent.atomic.AtomicBoolean(false)
    val calls = new java.util.concurrent.atomic.AtomicInteger(0)

    val reporter = PeriodicReporter.start(
      registry = registry,
      interval = Duration.ofMillis(20),
      format = MetricsFormat.Json,
      reset = false,
      initialDelay = Some(Duration.ofMillis(5)),
      threadName = "test-scalarometer-reporter"
    ) { payload =>
      threadIsDaemon.set(Thread.currentThread().isDaemon)
      val n = calls.incrementAndGet()
      payload shouldBe """{"ticks":1}"""
      tickLatch.countDown()
      if (n == 1) {
        throw new RuntimeException("transient sink failure")
      }
    }

    try {
      tickLatch.await(5, TimeUnit.SECONDS) shouldBe true
      threadIsDaemon.get() shouldBe true
      calls.get() should be >= 3
    } finally {
      reporter.close()
    }
    reporter.isClosed shouldBe true
  }

  it should "emit formatted log lines via PeriodicReporter.logging using java.util.logging" in {
    val registry = Registry()
    val g = Gauge.int("streams.active")(3)
    registry += g

    val captured = new CopyOnWriteArrayList[String]()
    val testLogger = Logger.getLogger("com.scalarometer.test.PeriodicReporterSpec")
    val handler = new Handler {
      override def publish(record: LogRecord): Unit = {
        val _ = captured.add(record.getMessage)
      }
      override def flush(): Unit = ()
      override def close(): Unit = ()
    }

    val prevUseParent = testLogger.getUseParentHandlers
    val prevLevel = testLogger.getLevel
    testLogger.setUseParentHandlers(false)
    testLogger.setLevel(Level.INFO)
    testLogger.addHandler(handler)

    val reporter = PeriodicReporter.logging(
      registry = registry,
      interval = 1.hour,
      format = MetricsFormat.Logfmt,
      reset = false,
      prefix = "[service:metrics] ",
      targetLogger = testLogger,
      level = Level.INFO
    )

    try {
      reporter.reportOnce() shouldBe Some("[service:metrics] streams.active=3")
      captured.size() shouldBe 1
      captured.get(0) shouldBe "[service:metrics] streams.active=3"
    } finally {
      reporter.close()
      testLogger.removeHandler(handler)
      testLogger.setUseParentHandlers(prevUseParent)
      testLogger.setLevel(prevLevel)
    }
  }
}
