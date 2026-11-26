package com.scalarometer.statistics

@SerialVersionUID(20200724110900L)
class AggregateCounter(name: String) extends Counter(name) {
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

  override def data(reset: Boolean = false): Map[String, Number] = synchronized {
    val result = Map[String, Number](
      _nameMax -> java.lang.Long.valueOf(_max),
      _nameMin -> java.lang.Long.valueOf(_min),
      _nameAvg -> java.lang.Double.valueOf(computeAverage(_count.get(), _samples))
    )
    if (reset) this.reset()
    result
  }

  def average: Double = synchronized {
    computeAverage(_count.get(), _samples)
  }

  private def computeAverage(total: Long, sampleCount: Long): Double =
    if (sampleCount > 0L) total.toDouble / sampleCount.toDouble
    else 0.0

  override def toString(): String = synchronized {
    s"${super.toString()} min: ${_min} max: ${_max} avg: ${computeAverage(_count.get(), _samples)}"
  }
}

object AggregateCounter {
  def apply(name: String): AggregateCounter = new AggregateCounter(name)
}
