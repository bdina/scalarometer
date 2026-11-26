package com.scalarometer.statistics

import scala.collection.mutable

@SerialVersionUID(20261126010000L)
open class AggregateCounter(name: String) extends Counter(name) {
  private var _min: Long = 0L
  private var _max: Long = 0L
  private var _samples: Long = 0L

  protected val _nameMin: String = s"$name.min"
  protected val _nameMax: String = s"$name.max"
  protected val _nameAvg: String = s"$name.avg"

  def min: Long = synchronized { _min }
  def max: Long = synchronized { _max }
  def samples: Long = synchronized { _samples }
  def isEmpty: Boolean = synchronized { _samples == 0L }
  def nonEmpty: Boolean = !isEmpty

  override def increment(count: Long = 1L): Long = synchronized {
    val updated = super.increment(count)
    if (_samples == 0L) {
      _min = count
      _max = count
    } else {
      if (count < _min) _min = count
      if (count > _max) _max = count
    }
    _samples += 1L
    updated
  }

  def resetAggregate(): Unit = synchronized {
    _min = 0L
    _max = 0L
    _samples = 0L
  }

  override def reset(): Unit = synchronized {
    super.reset()
    _min = 0L
    _max = 0L
    _samples = 0L
  }

  override def data(reset: Boolean = false): Map[String, Number] = {
    var total = 0L
    var curMin = 0L
    var curMax = 0L
    var curSamples = 0L
    synchronized {
      total = value
      curMin = _min
      curMax = _max
      curSamples = _samples
      if (reset) this.reset()
    }
    new scala.collection.immutable.Map.Map3(
      _nameMax,
      java.lang.Long.valueOf(curMax),
      _nameMin,
      java.lang.Long.valueOf(curMin),
      _nameAvg,
      java.lang.Double.valueOf(computeAverage(total, curSamples))
    )
  }

  override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
    var total = 0L
    var curMin = 0L
    var curMax = 0L
    var curSamples = 0L
    synchronized {
      total = value
      curMin = _min
      curMax = _max
      curSamples = _samples
      if (reset) this.reset()
    }
    val _ = builder
      .addOne((_nameMax, java.lang.Long.valueOf(curMax)))
      .addOne((_nameMin, java.lang.Long.valueOf(curMin)))
      .addOne((_nameAvg, java.lang.Double.valueOf(computeAverage(total, curSamples))))
  }

  def average: Double = {
    var total = 0L
    var curSamples = 0L
    synchronized {
      total = value
      curSamples = _samples
    }
    computeAverage(total, curSamples)
  }

  private def computeAverage(total: Long, sampleCount: Long): Double =
    if (sampleCount > 0L) total.toDouble / sampleCount.toDouble
    else 0.0

  override def toString(): String = {
    var total = 0L
    var curMin = 0L
    var curMax = 0L
    var curSamples = 0L
    synchronized {
      total = value
      curMin = _min
      curMax = _max
      curSamples = _samples
    }
    s"count: $total min: $curMin max: $curMax avg: ${computeAverage(total, curSamples)}"
  }
}

object AggregateCounter {
  def apply(name: String): AggregateCounter = new AggregateCounter(name)
}
