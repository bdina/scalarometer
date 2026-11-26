package com.scalarometer.statistics

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HistogramSpec extends AnyFlatSpec with Matchers {

  import Histogram.Percentile

  "Histogram" should "support default max_snapshots via companion apply" in {
    val h = Histogram("latency")
    h.name shouldBe "latency"
    h.max_snapshots shouldBe 1024
    h.isEmpty shouldBe true
    h.nonEmpty shouldBe false
    h.numberOfSnapshots shouldBe 0
    h.mean shouldBe 0.0
    h.variance shouldBe 0.0
    h.standard_deviation shouldBe 0.0
  }

  it should "record incremental snapshot deltas and respect max_snapshots cap" in {
    val h = Histogram("events", max_snapshots = 3)
    h.increment(10L)
    h.recordSnapshot()
    h.increment(15L)
    h.recordSnapshot()
    h.incrementByAndRecordSnapshot(5L) shouldBe 30L
    h.incrementAndRecordSnapshot()

    h.value shouldBe 31L
    h.numberOfSnapshots shouldBe 3
    h.values shouldBe Vector(10L, 15L, 5L)
    h.snapshots shouldBe Vector[Number](
      java.lang.Long.valueOf(10L),
      java.lang.Long.valueOf(15L),
      java.lang.Long.valueOf(5L)
    )
    h.snapshotsArray shouldBe Array(10L, 15L, 5L)
    h.snapshotsSortedArray shouldBe Array(5L, 10L, 15L)

    h.reset()
    h.isEmpty shouldBe true
    h.incrementByAndRecordSnapshot(7L) shouldBe 7L
    h.values shouldBe Vector(7L)
  }

  it should "compute mean, variance, standard deviation, and interpolated percentiles accurately" in {
    val h = Histogram("scores")
    Vector(10L, 20L, 30L, 40L, 50L).foreach(v => h.incrementByAndRecordSnapshot(v))

    h.mean shouldBe 30.0
    h.variance shouldBe 200.0
    h.standard_deviation shouldBe Math.sqrt(200.0) +- 1e-9

    h.percentile(Percentile.P_000).doubleValue() shouldBe 10.0 +- 1e-9
    h.percentile(Percentile.P_025).doubleValue() shouldBe 20.0 +- 1e-9
    h.percentile(Percentile.P_050).doubleValue() shouldBe 30.0 +- 1e-9
    h.percentile(Percentile.P_075).doubleValue() shouldBe 40.0 +- 1e-9
    h.percentile(Percentile.P_095).doubleValue() shouldBe 48.0 +- 1e-9
    h.percentile(Percentile.P_099).doubleValue() shouldBe 49.6 +- 1e-9
    h.percentile(Percentile.P_100).doubleValue() shouldBe 50.0 +- 1e-9

    h.percentile(Percentile.P_050, Array.emptyLongArray).doubleValue() shouldBe 0.0
    h.percentile(Percentile.P_099, Array(42L)).doubleValue() shouldBe 42.0
  }

  it should "emit all expected keys in data and atomically clear on reset" in {
    val h = Histogram("req.ms")
    Vector(2L, 4L, 6L, 8L).foreach(v => h.incrementByAndRecordSnapshot(v))

    val d = h.data(reset = true)
    d.keySet shouldBe Set(
      "req.ms",
      "req.ms.mean",
      "req.ms.stdv",
      "req.ms.0.0",
      "req.ms.0.25",
      "req.ms.0.5",
      "req.ms.0.75",
      "req.ms.0.95",
      "req.ms.0.99",
      "req.ms.1.0"
    )
    d("req.ms").longValue() shouldBe 20L
    d("req.ms.mean").doubleValue() shouldBe 5.0 +- 1e-9
    d("req.ms.0.5").doubleValue() shouldBe 5.0 +- 1e-9

    h.isEmpty shouldBe true
    h.value shouldBe 0L
  }

  it should "handle edge max_snapshots caps, lazy buffer growth, IArray snapshots, and appendData" in {
    val zeroCap = Histogram("zero.cap", max_snapshots = 0)
    zeroCap.incrementByAndRecordSnapshot(5L) shouldBe 5L
    zeroCap.isEmpty shouldBe true
    zeroCap.numberOfSnapshots shouldBe 0
    zeroCap.snapshotValues.length shouldBe 0
    zeroCap.percentileValue(Percentile.P_050) shouldBe 0.0

    val oneCap = Histogram("one.cap", max_snapshots = 1)
    oneCap.incrementByAndRecordSnapshot(9L) shouldBe 9L
    oneCap.incrementByAndRecordSnapshot(11L) shouldBe 20L
    oneCap.numberOfSnapshots shouldBe 1
    oneCap.snapshotValues.toVector shouldBe Vector(9L)
    oneCap.percentileValue(Percentile.P_099) shouldBe 9.0

    val growing = Histogram("grow", max_snapshots = 100)
    for (i <- 1 to 120) {
      growing.incrementByAndRecordSnapshot(i.toLong)
    }
    growing.numberOfSnapshots shouldBe 100
    growing.snapshotValues.length shouldBe 100
    growing.snapshotValues(0) shouldBe 1L
    growing.snapshotValues(99) shouldBe 100L
    growing.percentileValue(Percentile.P_000) shouldBe 1.0 +- 1e-9
    growing.percentileValue(Percentile.P_050) shouldBe 50.5 +- 1e-9
    growing.percentileValue(Percentile.P_100) shouldBe 100.0 +- 1e-9
    growing.percentileKey(Percentile.P_095) shouldBe "grow.0.95"

    val builder = Map.newBuilder[String, Number]
    growing.appendData(builder, reset = true)
    val built = builder.result()
    built("grow").longValue() shouldBe (1L to 120L).sum
    built("grow.0.5").doubleValue() shouldBe 50.5 +- 1e-9
    growing.isEmpty shouldBe true

    // Reuse internal buffer after reset with negative and positive deltas
    growing.increment(-10L)
    growing.recordSnapshot()
    growing.increment(30L)
    growing.recordSnapshot()
    growing.values shouldBe Vector(-10L, 30L)
    growing.mean shouldBe 10.0
  }

  it should "handle concurrent increments, snapshots, and atomic data resets safely" in {
    val h = Histogram("concurrent.reset.hist", max_snapshots = 2048)
    val threads = 4
    val perThread = 500
    val pool = Executors.newFixedThreadPool(threads)
    val startLatch = new CountDownLatch(1)
    val doneLatch = new CountDownLatch(threads)
    val collectedTotal = new java.util.concurrent.atomic.AtomicLong(0L)

    try {
      for (t <- 0 until threads) {
        pool.submit(new Runnable {
          override def run(): Unit = {
            startLatch.await()
            var i = 0
            while (i < perThread) {
              h.incrementByAndRecordSnapshot(2L)
              if (t == 0 && i % 50 == 0) {
                val snap = h.data(reset = true)
                collectedTotal.addAndGet(snap("concurrent.reset.hist").longValue())
              }
              i += 1
            }
            doneLatch.countDown()
          }
        })
      }
      startLatch.countDown()
      doneLatch.await(10, TimeUnit.SECONDS) shouldBe true
      collectedTotal.addAndGet(h.data(reset = true)("concurrent.reset.hist").longValue())
      collectedTotal.get() shouldBe (threads.toLong * perThread.toLong * 2L)
    } finally {
      pool.shutdownNow()
    }
  }

  it should "support Java serialization round-trip" in {
    val h = Histogram("ser.hist", max_snapshots = 16)
    h.incrementByAndRecordSnapshot(12L)
    h.incrementByAndRecordSnapshot(18L)

    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(h)
    oos.close()

    val ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray))
    val restored = ois.readObject().asInstanceOf[Histogram]
    ois.close()

    restored.name shouldBe "ser.hist"
    restored.max_snapshots shouldBe 16
    restored.values shouldBe Vector(12L, 18L)
    restored.mean shouldBe 15.0
  }

  it should "record snapshots safely under concurrent access" in {
    val h = Histogram("concurrent.hist", max_snapshots = 5000)
    val threads = 4
    val perThread = 500
    val pool = Executors.newFixedThreadPool(threads)
    val startLatch = new CountDownLatch(1)
    val doneLatch = new CountDownLatch(threads)

    try {
      for (_ <- 0 until threads) {
        pool.submit(new Runnable {
          override def run(): Unit = {
            startLatch.await()
            var i = 0
            while (i < perThread) {
              h.incrementByAndRecordSnapshot(1L)
              i += 1
            }
            doneLatch.countDown()
          }
        })
      }
      startLatch.countDown()
      doneLatch.await(10, TimeUnit.SECONDS) shouldBe true

      h.value shouldBe (threads.toLong * perThread.toLong)
      h.numberOfSnapshots shouldBe (threads * perThread)
      h.values.sum shouldBe h.value
    } finally {
      pool.shutdownNow()
    }
  }
}
