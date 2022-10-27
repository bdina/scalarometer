package com.statistics

import java.io.Serializable

class Counter(val name: String) extends Metric with CountableMetric with Serializable with StatisticsLogging {
  val serialVersionUID = 20200724110900L

  protected var _count: Long = 0L

  def count: Number = _count

  def increment(num: Long = 1L): Long = { _count += num ; _count }
  def decrement(num: Long = 1L): Long = { _count -= num ; _count }

  def decrementUnless(min: Long): Long =
    if (_count > min) decrement() else _count
  def decrementByUnless(num: Long, min: Long): Long = {
    val count = _count - num
    if (count > min) decrement(num) else _count
  }

  def reset(): Unit = _count = 0

  def data(reset: Boolean): Map[String, Number] = {
    val result = Map[String,Number](name -> _count)
    if (reset) this.reset()
    result
  }

  override def toString(): String = s"count: ${_count}"
}
object Counter {
  def apply(name: String) = new Counter(name)
}
