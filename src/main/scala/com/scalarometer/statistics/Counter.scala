package com.scalarometer.statistics

import java.io.Serializable
import java.lang.invoke.{MethodHandles, VarHandle}
import scala.annotation.{static, tailrec}
import scala.collection.mutable

@SerialVersionUID(20261126010000L)
open class Counter(val name: String) extends Metric with CountableMetric with Serializable with StatisticsLogging {
  @volatile private var _value: Long = 0L

  def value: Long = _value
  override def count: Number = java.lang.Long.valueOf(_value)

  def increment(num: Long = 1L): Long = {
    val prev: Long = Counter.ValueHandle.getAndAdd(this, num)
    prev + num
  }

  def decrement(num: Long = 1L): Long = {
    val prev: Long = Counter.ValueHandle.getAndAdd(this, -num)
    prev - num
  }

  def +=(num: Long): Long = increment(num)
  def -=(num: Long): Long = decrement(num)

  def decrementUnless(min: Long): Long = decrementByUnless(1L, min)

  def decrementByUnless(num: Long, min: Long): Long = {
    if (num <= 0L) {
      _value
    } else {
      @tailrec
      def loop(current: Long): Long = {
        val next = current - num
        val underflowed = ((current ^ num) & (current ^ next)) < 0L
        if (!underflowed && next >= min) {
          val witness: Long = Counter.ValueHandle.compareAndExchange(this, current, next)
          if (witness == current) next
          else loop(witness)
        } else {
          current
        }
      }
      loop(_value)
    }
  }

  override def reset(): Unit = {
    _value = 0L
  }

  protected def getAndResetValue(): Long =
    Counter.ValueHandle.getAndSet(this, 0L)

  override def data(reset: Boolean = false): Map[String, Number] = {
    val current = if (reset) getAndResetValue() else _value
    new scala.collection.immutable.Map.Map1(name, java.lang.Long.valueOf(current))
  }

  override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
    val current = if (reset) getAndResetValue() else _value
    val _ = builder.addOne((name, java.lang.Long.valueOf(current)))
  }

  override def toString(): String = s"count: ${_value}"
}

object Counter {
  @static private val ValueHandle: VarHandle =
    MethodHandles.lookup().findVarHandle(classOf[Counter], "_value", java.lang.Long.TYPE)

  def apply(name: String): Counter = new Counter(name)
}
