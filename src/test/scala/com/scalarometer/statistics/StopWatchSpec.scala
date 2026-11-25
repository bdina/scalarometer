package com.scalarometer.statistics

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
  }
}
