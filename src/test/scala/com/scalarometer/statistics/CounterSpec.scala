package com.scalarometer.statistics

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CounterSpec extends AnyFlatSpec with Matchers {

  "Counter" should "start at zero and support increment, decrement, and operators" in {
    val c = Counter("requests")
    c.name shouldBe "requests"
    c.value shouldBe 0L
    c.count.longValue() shouldBe 0L
    c.toString() shouldBe "count: 0"

    c.increment() shouldBe 1L
    c.increment(4L) shouldBe 5L
    (c += 5L) shouldBe 10L
    c.decrement() shouldBe 9L
    c.decrement(3L) shouldBe 6L
    (c -= 2L) shouldBe 4L
    c.value shouldBe 4L
    c.toString() shouldBe "count: 4"
  }

  it should "enforce minimum bounds consistently in decrementUnless and decrementByUnless" in {
    val c = Counter("slots")
    c.increment(3L) shouldBe 3L

    c.decrementUnless(1L) shouldBe 2L
    c.decrementUnless(1L) shouldBe 1L
    c.decrementUnless(1L) shouldBe 1L

    c.increment(4L) shouldBe 5L
    c.decrementByUnless(2L, 1L) shouldBe 3L
    c.decrementByUnless(2L, 1L) shouldBe 1L
    c.decrementByUnless(1L, 1L) shouldBe 1L
    c.decrementByUnless(0L, 0L) shouldBe 1L
    c.decrementByUnless(-5L, 0L) shouldBe 1L
  }

  it should "not underflow Long.MinValue in decrementByUnless" in {
    val c = Counter("edge")
    c.decrement(Long.MaxValue)
    c.decrement(1L)
    c.value shouldBe Long.MinValue
    c.decrementByUnless(10L, Long.MinValue) shouldBe Long.MinValue
  }

  it should "return data snapshots with optional atomic reset" in {
    val c = Counter("hits")
    c.increment(7L)

    c.data() shouldBe Map("hits" -> java.lang.Long.valueOf(7L))
    c.value shouldBe 7L

    c.data(reset = true) shouldBe Map("hits" -> java.lang.Long.valueOf(7L))
    c.value shouldBe 0L

    c.increment(3L)
    c.reset()
    c.value shouldBe 0L
  }

  it should "support Java serialization round-trip" in {
    val c = Counter("serializable")
    c.increment(42L)
    val _ = c.logger

    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(c)
    oos.close()

    val ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray))
    val restored = ois.readObject().asInstanceOf[Counter]
    ois.close()

    restored.name shouldBe "serializable"
    restored.value shouldBe 42L
    restored.logger should not be null
  }

  it should "handle concurrent increments and atomic resets without losing counts" in {
    val c = Counter("concurrent")
    val threads = 8
    val incrementsPerThread = 2000
    val pool = Executors.newFixedThreadPool(threads)
    val startLatch = new CountDownLatch(1)
    val doneLatch = new CountDownLatch(threads)
    val collected = new java.util.concurrent.atomic.AtomicLong(0L)

    try {
      for (i <- 0 until threads) {
        pool.submit(new Runnable {
          override def run(): Unit = {
            startLatch.await()
            var n = 0
            while (n < incrementsPerThread) {
              c.increment()
              if (i == 0 && n % 50 == 0) {
                collected.addAndGet(c.data(reset = true)("concurrent").longValue())
              }
              n += 1
            }
            doneLatch.countDown()
          }
        })
      }
      startLatch.countDown()
      doneLatch.await(10, TimeUnit.SECONDS) shouldBe true
      collected.addAndGet(c.data(reset = true)("concurrent").longValue())
      collected.get() shouldBe (threads.toLong * incrementsPerThread.toLong)
    } finally {
      pool.shutdownNow()
    }
  }
}
