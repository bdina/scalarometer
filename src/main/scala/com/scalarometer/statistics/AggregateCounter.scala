package com.statistics

class AggregateCounter(override val name: String) extends Counter(name) with StatisticsLogging {

  override val serialVersionUID = 20200724110900L

  protected var _min: Long = 0L
  protected var _max: Long = 0L
  protected var _avg: Long = 0L

  protected val _nameMin: String = s"$name.min"
  protected val _nameMax: String = s"$name.max"
  protected val _nameAvg: String = s"$name.avg"

  def min: Long = _min
  def max: Long = _max

  override def increment(count: Long = 1L): Long = {
    val _count = super.increment(count)
    _avg += 1

    if (_min == 0 || count < _min) _min = count
    if (_max == 0 || count > _max) _max = count

    _count
  }

  def resetAggregate(): Unit = {
    _min = 0L
    _max = 0L
    _avg = 0L
  }

  override def reset(): Unit = {
    super.reset()
    resetAggregate()
  }

  override def data(reset: Boolean): Map[String,Number] = {
    val data = Map[String,Number](_nameMax -> _max, _nameMin -> _min, _nameAvg -> average)
    if (reset) this.reset()
    data
  }

  def average: Double =
    if ((_count > 0) && (_avg > 0))
      _count.toDouble / _avg.toDouble
    else
      0.0

  override def toString(): String = s"${super.toString()} min: ${_min} max: ${_max} avg: ${_avg}"
}
object AggregateCounter {
  def apply(name: String) = new AggregateCounter(name)
}
