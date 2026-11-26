package com.scalarometer.statistics

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StopWatchSpec extends AnyFlatSpec with Matchers {

  import StopWatch.{Marker, State}

  "StopWatch" should "track start, mark, stop, elapsed, and reset deterministically" in {
    var nowNanos = 1_000_000_000L
    val sw = StopWatch(() => nowNanos)

    sw.state shouldBe State.STOPPED
    sw.isRunning shouldBe false
    sw.elapsed shouldBe 0L
    sw.elapsedNanos shouldBe 0L

    sw.start()
    sw.state shouldBe State.RUNNING
    sw.isRunning shouldBe true

    nowNanos += 25_000_000L
    sw.start()
    sw.elapsed shouldBe 25L
    sw.elapsedNanos shouldBe 25_000_000L
    sw.mark() shouldBe Marker(1_025_000_000L)

    nowNanos += 50_000_000L
    sw.stop() shouldBe 75L
    sw.state shouldBe State.STOPPED
    sw.isRunning shouldBe false
    sw.elapsed shouldBe 75L
    sw.mark() shouldBe Marker(1_075_000_000L)

    nowNanos += 100_000_000L
    sw.stop() shouldBe 75L
    sw.elapsed shouldBe 75L
    sw.toString() should include("STOPPED")
    sw.toString() should include("elapsed: 75")

    sw.reset()
    sw.elapsed shouldBe 0L
    sw.elapsedNanos shouldBe 0L
  }

  it should "restart cleanly after stop and support companion helpers" in {
    var nowNanos = 10_000_000L
    val sw = StopWatch(() => nowNanos)
    sw.start()
    nowNanos += 12_000_000L
    sw.stop() shouldBe 12L

    nowNanos += 100_000_000L
    sw.start()
    nowNanos += 30_000_000L
    sw.stop() shouldBe 30L

    val t0 = StopWatch.now
    val marker = Marker.fromNanoTime(t0)
    StopWatch.elapsed(t0) should be >= 0L
    StopWatch.elapsed(marker) should be >= 0L

    val (value, elapsedMs) = StopWatch.timed {
      21 * 2
    }
    value shouldBe 42
    elapsedMs should be >= 0L

    val (value2, elapsedNs) = StopWatch.timedNanos {
      "done"
    }
    value2 shouldBe "done"
    elapsedNs should be >= 0L
  }

  it should "expose unboxed Marker opaque type extensions and handle negative nanoTime clocks" in {
    val m = Marker.now
    m.timestamp should be <= System.nanoTime()
    m.elapsedNanos should be >= 0L
    m.elapsed should be >= 0L
    StopWatch.elapsed(m) should be >= 0L

    val markMethod = classOf[StopWatch].getMethod("mark")
    markMethod.getReturnType shouldBe java.lang.Long.TYPE

    var negClock = -5_000_000_000L
    val sw = StopWatch(() => negClock)
    sw.start()
    negClock += 40_000_000L
    sw.elapsed shouldBe 40L
    sw.mark().timestamp shouldBe -4_960_000_000L
    sw.stop() shouldBe 40L
    sw.elapsedNanos shouldBe 40_000_000L
  }

  it should "remain thread-safe under concurrent start, stop, mark, and reset calls" in {
    val sw = StopWatch()
    val threads = 6
    val iterations = 1000
    val pool = Executors.newFixedThreadPool(threads)
    val startLatch = new CountDownLatch(1)
    val doneLatch = new CountDownLatch(threads)

    try {
      for (t <- 0 until threads) {
        pool.submit(new Runnable {
          override def run(): Unit = {
            startLatch.await()
            var i = 0
            while (i < iterations) {
              (t + i) % 4 match {
                case 0 => sw.start()
                case 1 => val _ = sw.stop()
                case 2 => val _ = sw.mark()
                case _ => sw.reset()
              }
              i += 1
            }
            doneLatch.countDown()
          }
        })
      }
      startLatch.countDown()
      doneLatch.await(10, TimeUnit.SECONDS) shouldBe true
      sw.reset()
      sw.state shouldBe State.STOPPED
      sw.elapsedNanos shouldBe 0L
    } finally {
      pool.shutdownNow()
    }
  }
}
