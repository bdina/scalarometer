package com.scalarometer.statistics

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GaugeSpec extends AnyFlatSpec with Matchers {

  private def roundTrip(g: Gauge): Gauge = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(g)
    oos.close()

    val ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray))
    val restored = ois.readObject().asInstanceOf[Gauge]
    ois.close()
    restored
  }

  "Gauge" should "invoke its sampling function with name and reset flag" in {
    var observedReset = false
    val g = Gauge("queue.depth", (name, reset) => {
      observedReset = reset
      Map[String, Number](name -> java.lang.Integer.valueOf(if (reset) 0 else 12))
    })

    g.name shouldBe "queue.depth"
    g.toString() shouldBe "Gauge(queue.depth)"
    g.data() shouldBe Map("queue.depth" -> java.lang.Integer.valueOf(12))
    observedReset shouldBe false

    g.data(reset = true) shouldBe Map("queue.depth" -> java.lang.Integer.valueOf(0))
    observedReset shouldBe true

    val b = Map.newBuilder[String, Number]
    g.appendData(b, reset = false)
    b.result() shouldBe Map("queue.depth" -> java.lang.Integer.valueOf(12))
  }

  it should "evaluate Gauge.single dynamically on each read and support appendData" in {
    var current = 10
    val g = Gauge.single("active.workers")(java.lang.Integer.valueOf(current))

    g.data() shouldBe Map("active.workers" -> java.lang.Integer.valueOf(10))
    current = 25
    g.data(reset = true) shouldBe Map("active.workers" -> java.lang.Integer.valueOf(25))

    current = 30
    val b = Map.newBuilder[String, Number]
    g.appendData(b, reset = false)
    b.result() shouldBe Map("active.workers" -> java.lang.Integer.valueOf(30))
  }

  it should "evaluate specialized Gauge.long, Gauge.double, and Gauge.int factories dynamically" in {
    var bytes = 1024L
    var ratio = 0.75
    var threads = 4

    val gLong = Gauge.long("mem.bytes")(bytes)
    val gDouble = Gauge.double("cache.hit.ratio")(ratio)
    val gInt = Gauge.int("pool.threads")(threads)

    gLong.data() shouldBe Map("mem.bytes" -> java.lang.Long.valueOf(1024L))
    gDouble.data() shouldBe Map("cache.hit.ratio" -> java.lang.Double.valueOf(0.75))
    gInt.data() shouldBe Map("pool.threads" -> java.lang.Integer.valueOf(4))

    bytes = 2048L
    ratio = 0.9
    threads = 8

    val b = Map.newBuilder[String, Number]
    gLong.appendData(b, reset = false)
    gDouble.appendData(b, reset = false)
    gInt.appendData(b, reset = false)
    b.result() shouldBe Map(
      "mem.bytes" -> java.lang.Long.valueOf(2048L),
      "cache.hit.ratio" -> java.lang.Double.valueOf(0.9),
      "pool.threads" -> java.lang.Integer.valueOf(8)
    )
  }

  it should "support Java serialization round-trip across all Gauge factories" in {
    val fn: (String, Boolean) => Map[String, Number] =
      (name, _) => Map[String, Number](name -> java.lang.Long.valueOf(99L))

    val restoredMulti = roundTrip(Gauge("static.gauge", fn))
    restoredMulti.name shouldBe "static.gauge"
    restoredMulti.data() shouldBe Map("static.gauge" -> java.lang.Long.valueOf(99L))

    val restoredSingle = roundTrip(Gauge.single("single.gauge")(java.lang.Long.valueOf(7L)))
    restoredSingle.data() shouldBe Map("single.gauge" -> java.lang.Long.valueOf(7L))

    val restoredLong = roundTrip(Gauge.long("long.gauge")(123L))
    restoredLong.data() shouldBe Map("long.gauge" -> java.lang.Long.valueOf(123L))

    val restoredDouble = roundTrip(Gauge.double("double.gauge")(3.5))
    restoredDouble.data() shouldBe Map("double.gauge" -> java.lang.Double.valueOf(3.5))

    val restoredInt = roundTrip(Gauge.int("int.gauge")(42))
    restoredInt.data() shouldBe Map("int.gauge" -> java.lang.Integer.valueOf(42))
  }
}
