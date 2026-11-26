package com.scalarometer.statistics

import java.util.Arrays
import scala.collection.mutable

@SerialVersionUID(20261126010000L)
open class Histogram(name: String, val max_snapshots: Int = 1024) extends Counter(name) with SnapshotableMetric {
  import Histogram.Percentile

  private var buffer: Array[Long] = Array.emptyLongArray
  private var snapshotCount: Int = 0
  private var prev: Long = 0L
  private var capLogged: Boolean = false

  protected val nameMean: String = s"$name.mean"
  protected val nameStdv: String = s"$name.stdv"

  private val percentileKeys: Array[String] = {
    val all = Histogram.allPercentiles
    val keys = new Array[String](all.length)
    var i = 0
    while (i < all.length) {
      keys(i) = s"$name.${all(i).suffix}"
      i += 1
    }
    keys
  }

  def percentileKey(p: Percentile): String = percentileKeys(p.ordinal)

  def numberOfSnapshots: Int = synchronized { snapshotCount }
  def isEmpty: Boolean = synchronized { snapshotCount == 0 }
  def nonEmpty: Boolean = !isEmpty

  private def ensureCapacityLocked(): Unit = {
    val currentLen = buffer.length
    if (snapshotCount == currentLen) {
      val nextLen =
        if (currentLen == 0) math.min(Histogram.InitialBufferCapacity, max_snapshots)
        else if (currentLen >= max_snapshots / 2) max_snapshots
        else currentLen * 2
      buffer = Arrays.copyOf(buffer, nextLen)
    }
  }

  private def recordDeltaLocked(current: Long): Unit = {
    if (snapshotCount < max_snapshots) {
      ensureCapacityLocked()
      buffer(snapshotCount) = current - prev
      snapshotCount += 1
      prev = current
    } else if (!capLogged) {
      capLogged = true
      logFine(s"Histogram '$name' reached max_snapshots cap ($max_snapshots); dropping additional snapshots until reset")
    }
  }

  def recordSnapshot(): Unit = synchronized {
    recordDeltaLocked(value)
  }

  def incrementAndRecordSnapshot(): Unit = {
    val _ = incrementByAndRecordSnapshot(1L)
  }

  def incrementByAndRecordSnapshot(count: Long): Long = synchronized {
    val updated = increment(count)
    recordDeltaLocked(updated)
    updated
  }

  def snapshotsArray: Array[Long] = synchronized {
    if (snapshotCount == 0) Array.emptyLongArray
    else Arrays.copyOf(buffer, snapshotCount)
  }

  def snapshotValues: IArray[Long] = IArray.unsafeFromArray(snapshotsArray)

  def snapshotsSortedArray: Array[Long] = {
    val arr = snapshotsArray
    if (arr.length > 1) Arrays.sort(arr)
    arr
  }

  def values: Vector[Long] = {
    val arr = snapshotsArray
    val builder = Vector.newBuilder[Long]
    builder.sizeHint(arr.length)
    var i = 0
    while (i < arr.length) {
      val _ = builder.addOne(arr(i))
      i += 1
    }
    builder.result()
  }

  override def snapshots: Vector[Number] = {
    val arr = snapshotsArray
    val builder = Vector.newBuilder[Number]
    builder.sizeHint(arr.length)
    var i = 0
    while (i < arr.length) {
      val _ = builder.addOne(java.lang.Long.valueOf(arr(i)))
      i += 1
    }
    builder.result()
  }

  override def data(reset: Boolean = false): Map[String, Number] = {
    val builder = Map.newBuilder[String, Number]
    builder.sizeHint(3 + percentileKeys.length)
    appendData(builder, reset)
    builder.result()
  }

  override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
    var currentCount = 0L
    var snapshotCopy: Array[Long] = Array.emptyLongArray
    synchronized {
      currentCount = value
      if (snapshotCount > 0) {
        snapshotCopy = Arrays.copyOf(buffer, snapshotCount)
      }
      if (reset) this.reset()
    }
    val currentMean = computeMean(snapshotCopy)
    val currentStdv = Math.sqrt(computeVariance(snapshotCopy, currentMean))
    if (snapshotCopy.length > 1) Arrays.sort(snapshotCopy)

    val _ = builder
      .addOne((name, java.lang.Long.valueOf(currentCount)))
      .addOne((nameMean, java.lang.Double.valueOf(currentMean)))
      .addOne((nameStdv, java.lang.Double.valueOf(currentStdv)))

    val all = Histogram.allPercentiles
    var i = 0
    while (i < all.length) {
      val _ = builder.addOne((percentileKeys(i), java.lang.Double.valueOf(percentileValue(all(i), snapshotCopy))))
      i += 1
    }
  }

  override def reset(): Unit = synchronized {
    super.reset()
    prev = 0L
    snapshotCount = 0
    capLogged = false
  }

  def mean: Double = computeMean(snapshotsArray)

  def variance: Double = {
    val snapshotCopy = snapshotsArray
    computeVariance(snapshotCopy, computeMean(snapshotCopy))
  }

  def standard_deviation: Double = Math.sqrt(variance)

  def percentileValue(p: Percentile, counts: Array[Long]): Double = {
    val len = counts.length
    if (len == 0) {
      0.0
    } else if (len == 1) {
      counts(0).toDouble
    } else {
      val index = p.toDouble * (len - 1)
      val lowerIdx = index.toInt
      val frac = index - lowerIdx
      if (frac == 0.0 || lowerIdx >= len - 1) {
        counts(lowerIdx).toDouble
      } else {
        counts(lowerIdx) * (1.0 - frac) + counts(lowerIdx + 1) * frac
      }
    }
  }

  def percentileValue(p: Percentile): Double = percentileValue(p, snapshotsSortedArray)

  def percentile(p: Percentile, counts: Array[Long]): Number =
    java.lang.Double.valueOf(percentileValue(p, counts))

  def percentile(p: Percentile): Number =
    java.lang.Double.valueOf(percentileValue(p, snapshotsSortedArray))

  override def toString(): String = {
    var currentCount = 0L
    var snapshotCopy: Array[Long] = Array.emptyLongArray
    synchronized {
      currentCount = value
      if (snapshotCount > 0) {
        snapshotCopy = Arrays.copyOf(buffer, snapshotCount)
      }
    }
    val currentMean = computeMean(snapshotCopy)
    val currentStdv = Math.sqrt(computeVariance(snapshotCopy, currentMean))
    s"count: $currentCount mean: $currentMean standard deviation: $currentStdv"
  }

  private def computeMean(counts: Array[Long]): Double = {
    val len = counts.length
    if (len == 0) {
      0.0
    } else {
      var sum = 0.0
      var i = 0
      while (i < len) {
        sum += counts(i).toDouble
        i += 1
      }
      sum / len
    }
  }

  private def computeVariance(counts: Array[Long], currentMean: Double): Double = {
    val len = counts.length
    if (len == 0) {
      0.0
    } else {
      var sumSq = 0.0
      var i = 0
      while (i < len) {
        val diff = currentMean - counts(i).toDouble
        sumSq += diff * diff
        i += 1
      }
      sumSq / len
    }
  }
}

object Histogram {
  private val InitialBufferCapacity: Int = 16

  enum Percentile(val toDouble: Double, val suffix: String) derives CanEqual {
    case P_000 extends Percentile(0.00, "0.0")
    case P_025 extends Percentile(0.25, "0.25")
    case P_050 extends Percentile(0.50, "0.5")
    case P_075 extends Percentile(0.75, "0.75")
    case P_095 extends Percentile(0.95, "0.95")
    case P_099 extends Percentile(0.99, "0.99")
    case P_100 extends Percentile(1.00, "1.0")
  }

  val allPercentiles: IArray[Percentile] = IArray.unsafeFromArray(Percentile.values)

  def apply(name: String, max_snapshots: Int = 1024): Histogram = new Histogram(name, max_snapshots)
}
