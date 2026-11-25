package com.scalarometer.statistics

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AggregateCounterSpec extends AnyFlatSpec with Matchers {

  "AggregateCounter" should "start empty with zero min, max, samples, and average" in {
    val ac = AggregateCounter("latency")
    ac.name shouldBe "latency"
    ac.isEmpty shouldBe true
    ac.nonEmpty shouldBe false
    ac.samples shouldBe 0L
    ac.min shouldBe 0L
    ac.max shouldBe 0L
    ac.average shouldBe 0.0
  }

  it should "compute accurate min, max, average, and toString for positive samples" in {
    val ac = AggregateCounter("batch")
    ac.increment(10L) shouldBe 10L
    ac.increment(30L) shouldBe 40L
    ac.increment(20L) shouldBe 60L

    ac.nonEmpty shouldBe true
    ac.samples shouldBe 3L
    ac.min shouldBe 10L
    ac.max shouldBe 30L
    ac.average shouldBe 20.0
    ac.toString() shouldBe "count: 60 min: 10 max: 30 avg: 20.0"

    ac.data() shouldBe Map(
      "batch.min" -> java.lang.Long.valueOf(10L),
      "batch.max" -> java.lang.Long.valueOf(30L),
      "batch.avg" -> java.lang.Double.valueOf(20.0)
    )
  }

  it should "preserve zero and negative values in min, max, and average" in {
    val ac = AggregateCounter("delta")
    ac.increment(0L)
    ac.increment(5L)
    ac.min shouldBe 0L
    ac.max shouldBe 5L
    ac.average shouldBe 2.5

    val neg = AggregateCounter("neg")
    neg.increment(-10L)
    neg.increment(-4L)
    neg.min shouldBe -10L
    neg.max shouldBe -4L
    neg.average shouldBe -7.0

    val zeroAndNeg = AggregateCounter("zeroNeg")
    zeroAndNeg.increment(0L)
    zeroAndNeg.increment(-5L)
    zeroAndNeg.min shouldBe -5L
    zeroAndNeg.max shouldBe 0L
    zeroAndNeg.average shouldBe -2.5
  }

  it should "support resetAggregate, full reset, and data(reset = true)" in {
    val ac = AggregateCounter("ops")
    ac.increment(4L)
    ac.increment(8L)

    ac.resetAggregate()
    ac.value shouldBe 12L
    ac.samples shouldBe 0L
    ac.min shouldBe 0L
    ac.max shouldBe 0L
    ac.average shouldBe 0.0

    ac.reset()
    ac.value shouldBe 0L

    ac.increment(2L)
    ac.increment(6L)
    val snapshot = ac.data(reset = true)
    snapshot("ops.min").longValue() shouldBe 2L
    snapshot("ops.max").longValue() shouldBe 6L
    snapshot("ops.avg").doubleValue() shouldBe 4.0

    ac.isEmpty shouldBe true
    ac.value shouldBe 0L
  }

  it should "support Java serialization round-trip" in {
    val ac = AggregateCounter("ser")
    ac.increment(5L)
    ac.increment(15L)

    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(ac)
    oos.close()

    val ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray))
    val restored = ois.readObject().asInstanceOf[AggregateCounter]
    ois.close()

    restored.name shouldBe "ser"
    restored.min shouldBe 5L
    restored.max shouldBe 15L
    restored.average shouldBe 10.0
    restored.samples shouldBe 2L
  }

  it should "remain consistent under concurrent updates" in {
    val ac = AggregateCounter("concurrent")
    val threads = 6
    val perThread = 1000
    val pool = Executors.newFixedThreadPool(threads)
    val startLatch = new CountDownLatch(1)
    val doneLatch = new CountDownLatch(threads)

    try {
      for (t <- 1 to threads) {
        pool.submit(new Runnable {
          override def run(): Unit = {
            startLatch.await()
            var i = 0
            while (i < perThread) {
              ac.increment(t.toLong)
              i += 1
            }
            doneLatch.countDown()
          }
        })
      }
      startLatch.countDown()
      doneLatch.await(10, TimeUnit.SECONDS) shouldBe true

      ac.samples shouldBe (threads.toLong * perThread.toLong)
      ac.min shouldBe 1L
      ac.max shouldBe threads.toLong
      ac.average shouldBe ((threads + 1).toDouble / 2.0)
    } finally {
      pool.shutdownNow()
    }
  }
}
