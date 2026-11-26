package com.scalarometer.statistics.inlinecheck

import com.scalarometer.statistics.StopWatch
import com.scalarometer.statistics.StopWatch.Marker
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StopWatchInlineSpec extends AnyFlatSpec with Matchers {

  "StopWatch inline APIs" should "expand cleanly across package boundaries" in {
    val marker = Marker.now
    marker.timestamp should be <= System.nanoTime()
    marker.elapsed should be >= 0L
    marker.elapsedNanos should be >= 0L
    StopWatch.elapsed(marker) should be >= 0L

    val (r1, ms) = StopWatch.timed {
      10 + 32
    }
    r1 shouldBe 42
    ms should be >= 0L

    val (r2, ns) = StopWatch.timedNanos {
      "inline-ok"
    }
    r2 shouldBe "inline-ok"
    ns should be >= 0L
  }
}
