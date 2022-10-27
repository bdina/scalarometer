package com.statistics

class Histogram(override val name: String, val max_snapshots: Int = 1024) extends Counter(name) with SnapshotableMetric {
  import Histogram._

  override val serialVersionUID = 20200724110900L

  private var list_of_counts: Vector[Long] = Vector.empty

  private var prev = 0L

  protected val nameMean = s"$name.mean"
  protected val nameStdv = s"$name.stdv"

  protected val name000Percentile = s"$name.${Percentile.P_000.toDouble}"
  protected val name025Percentile = s"$name.${Percentile.P_025.toDouble}"
  protected val name050Percentile = s"$name.${Percentile.P_050.toDouble}"
  protected val name075Percentile = s"$name.${Percentile.P_075.toDouble}"
  protected val name095Percentile = s"$name.${Percentile.P_095.toDouble}"
  protected val name099Percentile = s"$name.${Percentile.P_099.toDouble}"
  protected val name100Percentile = s"$name.${Percentile.P_100.toDouble}"

  def numberOfSnapshots: Int = list_of_counts.size

  def recordSnapshot(): Unit =
    if (list_of_counts.size < max_snapshots) {
      list_of_counts = list_of_counts :+ (_count - prev)
      prev = _count
    }

  def incrementAndRecordSnapshot(): Unit = incrementByAndRecordSnapshot(1L)

  def incrementByAndRecordSnapshot(count: Long): Long = {
    val _count = increment(count)
    recordSnapshot()
    _count
  }

  def snapshots: Vector[Number] = list_of_counts.asInstanceOf[Vector[Number]]

  def snapshotsArray: Array[Long] = list_of_counts.toArray

  def snapshotsSortedArray: Array[Long] = snapshotsArray.sorted

  override def data(reset: Boolean): Map[String,Number] = {
    val sorted_values = snapshotsSortedArray
    ({
      val data = Map[String,Number](name -> _count, nameMean -> mean, nameStdv -> standard_deviation)
      if (reset) this.reset()
      data
    }) ++ Map[String,Number](
        name000Percentile -> percentile(Percentile.P_000, sorted_values)
      , name025Percentile -> percentile(Percentile.P_025, sorted_values)
      , name050Percentile -> percentile(Percentile.P_050, sorted_values)
      , name075Percentile -> percentile(Percentile.P_075, sorted_values)
      , name095Percentile -> percentile(Percentile.P_095, sorted_values)
      , name099Percentile -> percentile(Percentile.P_099, sorted_values)
      , name100Percentile -> percentile(Percentile.P_100, sorted_values)
    )
  }

  override def reset(): Unit = {
    super.reset()
    prev = 0L
    list_of_counts = Vector.empty
  }

  def mean: Double =
    if (list_of_counts.size < 1)
      0.0
    else
      list_of_counts.foldLeft (0.0) { case (acc,count) => acc + count } / list_of_counts.size

  def variance: Double =
    if (list_of_counts.size < 1)
      0.0
    else {
      val _mean = mean
      list_of_counts.foldLeft (0.0) { case (acc,count) =>  acc + (_mean - count) * (_mean - count) } / list_of_counts.size
    }

  def standard_deviation: Double = Math.sqrt(variance)

  def percentile(p: Percentile, counts: Array[Long]): Number = counts.size match {
    case 0 =>
      0.0
    case 1 =>
      counts(0).toDouble
    case _ =>
      val index = p.toDouble * (counts.length - 1.0)

      val floor   = Math.floor(index)
      val ceiling = Math.ceil(index)

      if (floor == ceiling)
        counts(index.toInt).toDouble
      else {
        val lower = counts(floor.toInt) * (ceiling - index)
        val upper = counts(ceiling.toInt) * (index - floor)
        lower + upper
      }
  }

  def percentile(p: Percentile): Number = percentile(p, list_of_counts.toArray.sorted)

  override def toString(): String = s"${super.toString()} mean: ${mean} standard deviation: ${standard_deviation}"
}
object Histogram {
  sealed trait Percentile {
    def toDouble: Double
  }
  object Percentile {
    case object P_000 extends Percentile {
      val toDouble = 0.00
    }
    case object P_025 extends Percentile {
      val toDouble = 0.25
    }
    case object P_050 extends Percentile {
      val toDouble = 0.50
    }
    case object P_075 extends Percentile {
      val toDouble = 0.75
    }
    case object P_095 extends Percentile {
      val toDouble = 0.95
    }
    case object P_099 extends Percentile {
      val toDouble = 0.99
    }
    case object P_100 extends Percentile {
      val toDouble = 1.00
    }
  }

  def apply(name: String, max_snapshots: Int) = new Histogram(name,max_snapshots)
}
