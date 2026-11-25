package com.scalarometer.statistics

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GaugeSpec extends AnyFlatSpec with Matchers {

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
  }

  it should "evaluate Gauge.single dynamically on each read" in {
    var current = 10
    val g = Gauge.single("active.workers")(java.lang.Integer.valueOf(current))

    g.data() shouldBe Map("active.workers" -> java.lang.Integer.valueOf(10))
    current = 25
    g.data(reset = true) shouldBe Map("active.workers" -> java.lang.Integer.valueOf(25))
  }

  it should "support Java serialization round-trip" in {
    val fn: (String, Boolean) => Map[String, Number] =
      (name, _) => Map[String, Number](name -> java.lang.Long.valueOf(99L))
    val g = Gauge("static.gauge", fn)

    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(g)
    oos.close()

    val ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray))
    val restored = ois.readObject().asInstanceOf[Gauge]
    ois.close()

    restored.name shouldBe "static.gauge"
    restored.data() shouldBe Map("static.gauge" -> java.lang.Long.valueOf(99L))
  }
}
