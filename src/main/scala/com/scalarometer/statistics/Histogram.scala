package com.scalarometer.statistics

@SerialVersionUID(20200724110900L)
class Histogram(name: String, val max_snapshots: Int = 1024) extends Counter(name) with SnapshotableMetric {
  import Histogram.Percentile

  private var list_of_counts: Vector[Long] = Vector.empty
  private var prev: Long = 0L
  private var capLogged: Boolean = false

  protected val nameMean: String = s"$name.mean"
  protected val nameStdv: String = s"$name.stdv"

  protected val name000Percentile: String = s"$name.${Percentile.P_000.toDouble}"
  protected val name025Percentile: String = s"$name.${Percentile.P_025.toDouble}"
  protected val name050Percentile: String = s"$name.${Percentile.P_050.toDouble}"
  protected val name075Percentile: String = s"$name.${Percentile.P_075.toDouble}"
  protected val name095Percentile: String = s"$name.${Percentile.P_095.toDouble}"
  protected val name099Percentile: String = s"$name.${Percentile.P_099.toDouble}"
  protected val name100Percentile: String = s"$name.${Percentile.P_100.toDouble}"

  private val percentileNames: Array[(Percentile, String)] =
    Percentile.values.map(p => p -> s"$name.${p.toDouble}")

  def numberOfSnapshots: Int = synchronized { list_of_counts.size }
  def isEmpty: Boolean = synchronized { list_of_counts.isEmpty }
  def nonEmpty: Boolean = !isEmpty

  def recordSnapshot(): Unit = synchronized {
    if (list_of_counts.size < max_snapshots) {
      val current = _count.get()
      list_of_counts = list_of_counts :+ (current - prev)
      prev = current
    } else if (!capLogged) {
      capLogged = true
      logger.fine(s"Histogram '$name' reached max_snapshots cap ($max_snapshots); dropping additional snapshots until reset")
    }
  }

  def incrementAndRecordSnapshot(): Unit = {
    val _ = incrementByAndRecordSnapshot(1L)
  }

  def incrementByAndRecordSnapshot(count: Long): Long = synchronized {
    val updated = increment(count)
    recordSnapshot()
    updated
  }

  def values: Vector[Long] = synchronized { list_of_counts }

  override def snapshots: Vector[Number] = synchronized {
    list_of_counts.map(java.lang.Long.valueOf)
  }

  def snapshotsArray: Array[Long] = synchronized { list_of_counts.toArray }

  def snapshotsSortedArray: Array[Long] = snapshotsArray.sorted

  override def data(reset: Boolean = false): Map[String, Number] = {
    val (currentCount, snapshotCounts) = synchronized {
      val snapshot = (_count.get(), list_of_counts)
      if (reset) this.reset()
      snapshot
    }
    val sortedValues = snapshotCounts.toArray.sorted
    val currentMean = computeMean(snapshotCounts)
    val currentStdv = Math.sqrt(computeVariance(snapshotCounts, currentMean))
    val base = Map[String, Number](
      name -> java.lang.Long.valueOf(currentCount),
      nameMean -> java.lang.Double.valueOf(currentMean),
      nameStdv -> java.lang.Double.valueOf(currentStdv)
    )
    percentileNames.foldLeft(base) { (acc, entry) =>
      val (p, key) = entry
      acc.updated(key, percentile(p, sortedValues))
    }
  }

  override def reset(): Unit = synchronized {
    super.reset()
    prev = 0L
    list_of_counts = Vector.empty
    capLogged = false
  }

  def mean: Double = {
    val snapshotCounts = synchronized { list_of_counts }
    computeMean(snapshotCounts)
  }

  def variance: Double = {
    val snapshotCounts = synchronized { list_of_counts }
    computeVariance(snapshotCounts, computeMean(snapshotCounts))
  }

  def standard_deviation: Double = Math.sqrt(variance)

  def percentile(p: Percentile, counts: Array[Long]): Number = counts.length match {
    case 0 =>
      java.lang.Double.valueOf(0.0)
    case 1 =>
      java.lang.Double.valueOf(counts(0).toDouble)
    case len =>
      val index = p.toDouble * (len - 1.0)
      val floor = Math.floor(index)
      val ceiling = Math.ceil(index)

      if (floor == ceiling) {
        java.lang.Double.valueOf(counts(index.toInt).toDouble)
      } else {
        val lower = counts(floor.toInt) * (ceiling - index)
        val upper = counts(ceiling.toInt) * (index - floor)
        java.lang.Double.valueOf(lower + upper)
      }
  }

  def percentile(p: Percentile): Number = percentile(p, snapshotsSortedArray)

  override def toString(): String = {
    val (currentCount, snapshotCounts) = synchronized { (_count.get(), list_of_counts) }
    val currentMean = computeMean(snapshotCounts)
    val currentStdv = Math.sqrt(computeVariance(snapshotCounts, currentMean))
    s"count: $currentCount mean: $currentMean standard deviation: $currentStdv"
  }

  private def computeMean(counts: Vector[Long]): Double =
    if (counts.isEmpty) 0.0
    else counts.foldLeft(0.0)((acc, count) => acc + count.toDouble) / counts.size

  private def computeVariance(counts: Vector[Long], currentMean: Double): Double =
    if (counts.isEmpty) 0.0
    else counts.foldLeft(0.0)((acc, count) => acc + (currentMean - count.toDouble) * (currentMean - count.toDouble)) / counts.size
}

object Histogram {
  enum Percentile(val toDouble: Double) {
    case P_000 extends Percentile(0.00)
    case P_025 extends Percentile(0.25)
    case P_050 extends Percentile(0.50)
    case P_075 extends Percentile(0.75)
    case P_095 extends Percentile(0.95)
    case P_099 extends Percentile(0.99)
    case P_100 extends Percentile(1.00)
  }

  def apply(name: String, max_snapshots: Int = 1024): Histogram = new Histogram(name, max_snapshots)
}
