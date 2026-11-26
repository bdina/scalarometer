package com.scalarometer.statistics

import java.io.StringWriter
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MetricsFormatSpec extends AnyFlatSpec with Matchers {

  "MetricsFormat.Logfmt" should "format empty and populated registries in deterministic key order while respecting resettable vs fixed buckets" in {
    val empty = Registry()
    MetricsFormat.Logfmt.contentType shouldBe "text/plain; charset=utf-8"
    MetricsFormat.Logfmt.format(empty) shouldBe ""

    val registry = Registry()
    val requests = Counter("http.requests")
    val batch = AggregateCounter("batch.size")
    val workers = Gauge.int("workers.active")(8)
    val uptime = Counter("service.uptime")

    requests.increment(12L)
    batch.increment(10L)
    batch.increment(30L)
    uptime.increment(500L)

    registry ++= List(requests, batch, workers)
    registry.registerFixedCounter(uptime)

    MetricsFormat.Logfmt.format(registry, reset = false) shouldBe
      "batch.size.avg=20.0 batch.size.max=30 batch.size.min=10 http.requests=12 service.uptime=500 workers.active=8"

    MetricsFormat.Logfmt.format(registry, reset = true) shouldBe
      "batch.size.avg=20.0 batch.size.max=30 batch.size.min=10 http.requests=12 service.uptime=500 workers.active=8"

    // Resettable metrics cleared while fixed metrics keep accumulating
    MetricsFormat.Logfmt.format(registry, reset = false) shouldBe
      "batch.size.avg=0.0 batch.size.max=0 batch.size.min=0 http.requests=0 service.uptime=500 workers.active=8"
  }

  it should "format unregistered session metrics, custom Appendables, special floats, and unsafe keys" in {
    val bytes = Counter("stream.bytes")
    val hist = Histogram("stream.chunk.ms")
    bytes.increment(4096L)
    hist.incrementByAndRecordSnapshot(5L)

    val sessionLine = MetricsFormat.Logfmt.formatMetrics(List(bytes, hist))
    sessionLine should include("stream.bytes=4096")
    sessionLine should include("stream.chunk.ms=5")
    sessionLine should include("stream.chunk.ms.mean=5.0")
    sessionLine should include("stream.chunk.ms.0.95=5.0")

    val writer = new StringWriter()
    MetricsFormat.Logfmt.writeStats(
      Map[String, Number](
        "bad key=1" -> java.lang.Double.valueOf(Double.NaN),
        "pos.inf" -> java.lang.Double.valueOf(Double.PositiveInfinity),
        "neg.inf" -> java.lang.Double.valueOf(Double.NegativeInfinity),
        "flt" -> java.lang.Float.valueOf(1.5f),
        "sht" -> java.lang.Short.valueOf(3.toShort),
        "byt" -> java.lang.Byte.valueOf(2.toByte)
      ),
      writer
    )
    writer.toString shouldBe "bad_key_1=NaN byt=2 flt=1.5 neg.inf=-Inf pos.inf=+Inf sht=3"
  }

  "MetricsFormat.Json" should "format empty and populated registries as valid sorted RFC 8259 JSON" in {
    val empty = Registry()
    MetricsFormat.Json.contentType shouldBe "application/json; charset=utf-8"
    MetricsFormat.Json.format(empty) shouldBe "{}"

    val registry = Registry()
    val c = Counter("http.requests")
    val g = Gauge.double("cpu.load")(0.42)
    val fixed = Counter("uptime.ticks")
    c.increment(7L)
    fixed.increment(99L)
    registry += c += g
    registry.registerFixedCounter(fixed)

    MetricsFormat.Json.format(registry, reset = true) shouldBe
      """{"cpu.load":0.42,"http.requests":7,"uptime.ticks":99}"""

    MetricsFormat.Json.format(registry, reset = false) shouldBe
      """{"cpu.load":0.42,"http.requests":0,"uptime.ticks":99}"""
  }

  it should "escape control characters and quotes in keys and render non-finite floats as null" in {
    val writer = new StringWriter()
    MetricsFormat.Json.writeStats(
      Map[String, Number](
        "a\"b\\c\n\t\u0001" -> java.lang.Long.valueOf(1L),
        "nan" -> java.lang.Double.valueOf(Double.NaN),
        "inf" -> java.lang.Double.valueOf(Double.PositiveInfinity),
        "finf" -> java.lang.Float.valueOf(Float.NegativeInfinity),
        "ok" -> java.lang.Integer.valueOf(42)
      ),
      writer
    )
    writer.toString shouldBe
      ("""{"a\"b\\c\n\t\""" + """u0001":1,"finf":null,"inf":null,"nan":null,"ok":42}""")
  }

  it should "keep last value consistently when fixed and resettable metrics emit the same key" in {
    val registry = Registry()
    val resettable = Counter("shared.key")
    val fixed = Counter("shared.key")
    resettable.increment(1L)
    fixed.increment(9L)
    registry += resettable
    registry.registerFixedCounter(fixed)

    MetricsFormat.Logfmt.format(registry) shouldBe "shared.key=9"
    MetricsFormat.Json.format(registry) shouldBe """{"shared.key":9}"""
  }
}
