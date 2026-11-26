package com.scalarometer.statistics.inlinecheck

import com.scalarometer.statistics.{Counter, Gauge, MetricsFormat, Registry, StopWatch}
import com.scalarometer.statistics.StopWatch.Marker
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StopWatchInlineSpec extends AnyFlatSpec with Matchers {

  "StopWatch, Gauge, and Registry inline APIs" should "expand cleanly across package boundaries" in {
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

    val gLong = Gauge.long("ext.long")(99L)
    val gDouble = Gauge.double("ext.double")(2.5)
    val gInt = Gauge.int("ext.int")(7)
    gLong.data()("ext.long").longValue() shouldBe 99L
    gDouble.data()("ext.double").doubleValue() shouldBe 2.5
    gInt.data()("ext.int").intValue() shouldBe 7

    val registry = Registry()
    val scopedCounter = Counter("ext.scoped")
    val formatted = registry.withScope(scopedCounter) {
      val _ = (scopedCounter += 3L)
      MetricsFormat.Logfmt.format(registry)
    }
    formatted shouldBe "ext.scoped=3"
    registry.isEmpty shouldBe true
  }
}
