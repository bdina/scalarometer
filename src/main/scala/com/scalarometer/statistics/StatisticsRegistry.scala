package com.scalarometer.statistics

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec

final class MetricScope private[statistics] (
    private val registry: Registry,
    private val registered: IArray[Metric]
) extends AutoCloseable {
  private val closed: AtomicBoolean = new AtomicBoolean(false)

  def isClosed: Boolean = closed.get()
  def metrics: IArray[Metric] = registered

  override def close(): Unit = {
    if (closed.compareAndSet(false, true) && registered.length > 0) {
      registry.unregisterScoped(registered)
    }
  }
}

open class Registry extends Statistics with Iterable[Metric] {
  import Registry.{EmptyBucket, MetricBucket}

  private val countersRef: AtomicReference[MetricBucket] = new AtomicReference(EmptyBucket)
  private val fixedCountersRef: AtomicReference[MetricBucket] = new AtomicReference(EmptyBucket)

  override protected def readerBean(): ReaderBean = ReaderBean(this)

  def metrics: Set[Metric] = countersRef.get().metrics
  def fixedMetrics: Set[Metric] = fixedCountersRef.get().metrics

  def allMetrics: Set[Metric] = {
    val resettable = countersRef.get().metrics
    val fixed = fixedCountersRef.get().metrics
    if (fixed.isEmpty) resettable
    else if (resettable.isEmpty) fixed
    else resettable ++ fixed
  }

  override def isEmpty: Boolean =
    countersRef.get().metrics.isEmpty && fixedCountersRef.get().metrics.isEmpty

  override def size: Int = {
    val resettable = countersRef.get().metrics
    val fixed = fixedCountersRef.get().metrics
    if (fixed.isEmpty) resettable.size
    else if (resettable.isEmpty) fixed.size
    else {
      var count = resettable.size
      val it = fixed.iterator
      while (it.hasNext) {
        if (!resettable.contains(it.next())) count += 1
      }
      count
    }
  }

  override def iterator: Iterator[Metric] = {
    val resettable = countersRef.get().metrics
    val fixed = fixedCountersRef.get().metrics
    if (fixed.isEmpty) resettable.iterator
    else if (resettable.isEmpty) fixed.iterator
    else resettable.iterator ++ fixed.iterator.filterNot(resettable.contains)
  }

  override def foreach[U](f: Metric => U): Unit = {
    val resettable = countersRef.get().metrics
    val fixed = fixedCountersRef.get().metrics
    resettable.foreach(f)
    if (resettable.isEmpty) {
      fixed.foreach(f)
    } else if (fixed.nonEmpty) {
      val it = fixed.iterator
      while (it.hasNext) {
        val m = it.next()
        if (!resettable.contains(m)) {
          val _ = f(m)
        }
      }
    }
  }

  def stats(reset: Boolean = false): Map[String, Number] = {
    val resettable = countersRef.get().metrics
    val fixed = fixedCountersRef.get().metrics
    if (resettable.isEmpty && fixed.isEmpty) {
      Map.empty
    } else {
      val builder = Map.newBuilder[String, Number]
      builder.sizeHint(resettable.size + fixed.size)
      val it1 = resettable.iterator
      while (it1.hasNext) {
        it1.next().appendData(builder, reset)
      }
      val it2 = fixed.iterator
      while (it2.hasNext) {
        it2.next().appendData(builder, reset = false)
      }
      builder.result()
    }
  }

  def registerCounter(counter: Counter): Set[Metric] = registerMetric(counter)
  def unregisterCounter(counter: Counter): Set[Metric] = unregisterMetric(counter)
  def registerFixedCounter(counter: Counter): Set[Metric] = registerFixedMetric(counter)

  def registerMetric(metric: Metric): Set[Metric] = {
    val updated = registerIn(countersRef, metric)
    logFine(s"Registered metric '${metric.name}' (total resettable: ${updated.size})")
    updated
  }

  def unregisterMetric(metric: Metric): Set[Metric] = {
    val updated = unregisterFrom(countersRef, metric)
    logFine(s"Unregistered metric '${metric.name}' (total resettable: ${updated.size})")
    updated
  }

  def registerFixedMetric(metric: Metric): Set[Metric] = {
    val updated = registerIn(fixedCountersRef, metric)
    logFine(s"Registered fixed metric '${metric.name}' (total fixed: ${updated.size})")
    updated
  }

  def unregisterFixedMetric(metric: Metric): Set[Metric] = {
    val updated = unregisterFrom(fixedCountersRef, metric)
    logFine(s"Unregistered fixed metric '${metric.name}' (total fixed: ${updated.size})")
    updated
  }

  def +=(metric: Metric): this.type = {
    val _ = registerMetric(metric)
    this
  }

  def -=(metric: Metric): this.type = {
    val _ = unregisterMetric(metric)
    val _ = unregisterFixedMetric(metric)
    this
  }

  def ++=(items: IterableOnce[Metric]): this.type = {
    val materialized = materialize(items)
    val updated = registerAllIn(countersRef, materialized)
    logFine(s"Bulk registered metrics (total resettable: ${updated.size})")
    this
  }

  def --=(items: IterableOnce[Metric]): this.type = {
    val materialized = materialize(items)
    val updatedResettable = unregisterAllFrom(countersRef, materialized)
    val updatedFixed = unregisterAllFrom(fixedCountersRef, materialized)
    logFine(s"Bulk unregistered metrics (total resettable: ${updatedResettable.size}, total fixed: ${updatedFixed.size})")
    this
  }

  def scoped(metrics: Metric*): MetricScope = scopedAll(metrics)

  def scopedAll(items: IterableOnce[Metric]): MetricScope = {
    val arr = IArray.from(items)
    if (arr.length > 0) {
      val updated = registerArrayIn(countersRef, arr)
      logFine(s"Scoped registered ${arr.length} metric(s) (total resettable: ${updated.size})")
    }
    new MetricScope(this, arr)
  }

  inline def withScope[A](metrics: Metric*)(inline block: A): A = {
    val scope = scoped(metrics*)
    try {
      block
    } finally {
      scope.close()
    }
  }

  private[statistics] def unregisterScoped(items: IArray[Metric]): Unit = {
    val updatedResettable = unregisterArrayFrom(countersRef, items)
    val updatedFixed = unregisterArrayFrom(fixedCountersRef, items)
    logFine(s"Closed metric scope (${items.length} metric(s); remaining resettable: ${updatedResettable.size}, fixed: ${updatedFixed.size})")
  }

  def clear(): Unit = {
    countersRef.set(EmptyBucket)
    fixedCountersRef.set(EmptyBucket)
    logFine("Cleared all metrics from registry")
  }

  def metric(name: String): Option[Metric] = countersRef.get().byName.get(name)
  def fixedMetric(name: String): Option[Metric] = fixedCountersRef.get().byName.get(name)

  def get(name: String): Option[Metric] = {
    val found = countersRef.get().byName.get(name)
    if (found.isDefined) found else fixedCountersRef.get().byName.get(name)
  }

  def contains(name: String): Boolean =
    countersRef.get().byName.contains(name) || fixedCountersRef.get().byName.contains(name)

  override def toString(): String =
    s"Registry(metrics=${countersRef.get().metrics.size}, fixedMetrics=${fixedCountersRef.get().metrics.size})"

  private def materialize(items: IterableOnce[Metric]): Iterable[Metric] = items match {
    case iterable: Iterable[Metric @unchecked] => iterable
    case other                                 => other.iterator.toVector
  }

  @tailrec
  private def registerIn(ref: AtomicReference[MetricBucket], metric: Metric): Set[Metric] = {
    val current = ref.get()
    val next = current.register(metric)
    if ((next eq current) || ref.compareAndSet(current, next)) next.metrics
    else registerIn(ref, metric)
  }

  @tailrec
  private def unregisterFrom(ref: AtomicReference[MetricBucket], metric: Metric): Set[Metric] = {
    val current = ref.get()
    val next = current.unregister(metric)
    if ((next eq current) || ref.compareAndSet(current, next)) next.metrics
    else unregisterFrom(ref, metric)
  }

  @tailrec
  private def registerAllIn(ref: AtomicReference[MetricBucket], items: Iterable[Metric]): Set[Metric] = {
    val current = ref.get()
    val next = current.registerAll(items)
    if ((next eq current) || ref.compareAndSet(current, next)) next.metrics
    else registerAllIn(ref, items)
  }

  @tailrec
  private def unregisterAllFrom(ref: AtomicReference[MetricBucket], items: Iterable[Metric]): Set[Metric] = {
    val current = ref.get()
    val next = current.unregisterAll(items)
    if ((next eq current) || ref.compareAndSet(current, next)) next.metrics
    else unregisterAllFrom(ref, items)
  }

  @tailrec
  private def registerArrayIn(ref: AtomicReference[MetricBucket], items: IArray[Metric]): Set[Metric] = {
    val current = ref.get()
    val next = current.registerArray(items)
    if ((next eq current) || ref.compareAndSet(current, next)) next.metrics
    else registerArrayIn(ref, items)
  }

  @tailrec
  private def unregisterArrayFrom(ref: AtomicReference[MetricBucket], items: IArray[Metric]): Set[Metric] = {
    val current = ref.get()
    val next = current.unregisterArray(items)
    if ((next eq current) || ref.compareAndSet(current, next)) next.metrics
    else unregisterArrayFrom(ref, items)
  }
}

object Registry {
  private final class MetricBucket(val metrics: Set[Metric], val byName: Map[String, Metric]) {
    def register(metric: Metric): MetricBucket = {
      if (metrics.contains(metric)) this
      else new MetricBucket(metrics + metric, byName.updated(metric.name, metric))
    }

    def unregister(metric: Metric): MetricBucket = {
      if (!metrics.contains(metric)) {
        this
      } else {
        val updatedMetrics = metrics - metric
        val updatedByName = byName.get(metric.name) match {
          case Some(existing) if existing eq metric =>
            updatedMetrics.find(_.name == metric.name) match {
              case Some(replacement) => byName.updated(metric.name, replacement)
              case None              => byName - metric.name
            }
          case _ => byName
        }
        new MetricBucket(updatedMetrics, updatedByName)
      }
    }

    def registerAll(items: Iterable[Metric]): MetricBucket = {
      val it = items.iterator
      var curMetrics = metrics
      var curByName = byName
      var changed = false
      while (it.hasNext) {
        val m = it.next()
        if (!curMetrics.contains(m)) {
          curMetrics = curMetrics + m
          curByName = curByName.updated(m.name, m)
          changed = true
        }
      }
      if (changed) new MetricBucket(curMetrics, curByName) else this
    }

    def unregisterAll(items: Iterable[Metric]): MetricBucket = {
      val it = items.iterator
      var cur = this
      while (it.hasNext) {
        cur = cur.unregister(it.next())
      }
      cur
    }

    def registerArray(items: IArray[Metric]): MetricBucket = {
      var curMetrics = metrics
      var curByName = byName
      var changed = false
      var i = 0
      while (i < items.length) {
        val m = items(i)
        if (!curMetrics.contains(m)) {
          curMetrics = curMetrics + m
          curByName = curByName.updated(m.name, m)
          changed = true
        }
        i += 1
      }
      if (changed) new MetricBucket(curMetrics, curByName) else this
    }

    def unregisterArray(items: IArray[Metric]): MetricBucket = {
      var cur = this
      var i = 0
      while (i < items.length) {
        cur = cur.unregister(items(i))
        i += 1
      }
      cur
    }
  }

  private val EmptyBucket: MetricBucket = new MetricBucket(Set.empty, Map.empty)

  val global: Registry = Registry()
  def apply(): Registry = new Registry()
}
