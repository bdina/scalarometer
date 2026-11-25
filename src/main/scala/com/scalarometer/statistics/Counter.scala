package com.scalarometer.statistics

import java.io.Serializable
import java.util.concurrent.atomic.AtomicLong
import scala.annotation.tailrec

@SerialVersionUID(20200724110900L)
class Counter(val name: String) extends Metric with CountableMetric with Serializable with StatisticsLogging {
  protected val _count: AtomicLong = new AtomicLong(0L)

  def value: Long = _count.get()
  override def count: Number = java.lang.Long.valueOf(_count.get())

  def increment(num: Long = 1L): Long = _count.addAndGet(num)
  def decrement(num: Long = 1L): Long = _count.addAndGet(-num)

  def +=(num: Long): Long = increment(num)
  def -=(num: Long): Long = decrement(num)

  def decrementUnless(min: Long): Long = decrementByUnless(1L, min)

  def decrementByUnless(num: Long, min: Long): Long = {
    if (num <= 0L) {
      _count.get()
    } else {
      @tailrec
      def loop(): Long = {
        val current = _count.get()
        val next = current - num
        val underflowed = ((current ^ num) & (current ^ next)) < 0L
        if (!underflowed && next >= min) {
          if (_count.compareAndSet(current, next)) next
          else loop()
        } else {
          current
        }
      }
      loop()
    }
  }

  override def reset(): Unit = _count.set(0L)

  override def data(reset: Boolean = false): Map[String, Number] = {
    val current = if (reset) _count.getAndSet(0L) else _count.get()
    Map[String, Number](name -> java.lang.Long.valueOf(current))
  }

  override def toString(): String = s"count: ${_count.get()}"
}

object Counter {
  def apply(name: String): Counter = new Counter(name)
}
