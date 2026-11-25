package com.scalarometer.statistics

import java.util.concurrent.atomic.AtomicReference

class Registry extends Statistics with Iterable[Metric] {
  private val countersRef: AtomicReference[Set[Metric]] = new AtomicReference(Set.empty)
  private val fixedCountersRef: AtomicReference[Set[Metric]] = new AtomicReference(Set.empty)

  override protected def readerBean(): ReaderBean = ReaderBean(this)

  def metrics: Set[Metric] = countersRef.get()
  def fixedMetrics: Set[Metric] = fixedCountersRef.get()
  def allMetrics: Set[Metric] = countersRef.get() ++ fixedCountersRef.get()

  override def iterator: Iterator[Metric] = allMetrics.iterator

  def stats(reset: Boolean = false): Map[String, Number] = {
    val resettableStats = countersRef.get().foldLeft(Map.empty[String, Number]) { (acc, m) =>
      acc ++ m.data(reset)
    }
    fixedCountersRef.get().foldLeft(resettableStats) { (acc, m) =>
      acc ++ m.data(reset = false)
    }
  }

  def registerCounter(counter: Counter): Set[Metric] = registerMetric(counter)
  def unregisterCounter(counter: Counter): Set[Metric] = unregisterMetric(counter)
  def registerFixedCounter(counter: Counter): Set[Metric] = registerFixedMetric(counter)

  def registerMetric(metric: Metric): Set[Metric] = {
    val updated = countersRef.updateAndGet(_ + metric)
    logger.fine(s"Registered metric '${metric.name}' (total resettable: ${updated.size})")
    updated
  }

  def unregisterMetric(metric: Metric): Set[Metric] = {
    val updated = countersRef.updateAndGet(_ - metric)
    logger.fine(s"Unregistered metric '${metric.name}' (total resettable: ${updated.size})")
    updated
  }

  def registerFixedMetric(metric: Metric): Set[Metric] = {
    val updated = fixedCountersRef.updateAndGet(_ + metric)
    logger.fine(s"Registered fixed metric '${metric.name}' (total fixed: ${updated.size})")
    updated
  }

  def unregisterFixedMetric(metric: Metric): Set[Metric] = {
    val updated = fixedCountersRef.updateAndGet(_ - metric)
    logger.fine(s"Unregistered fixed metric '${metric.name}' (total fixed: ${updated.size})")
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

  def clear(): Unit = {
    countersRef.set(Set.empty)
    fixedCountersRef.set(Set.empty)
    logger.fine("Cleared all metrics from registry")
  }

  def metric(name: String): Option[Metric] = countersRef.get().find(_.name == name)
  def fixedMetric(name: String): Option[Metric] = fixedCountersRef.get().find(_.name == name)
  def get(name: String): Option[Metric] = metric(name).orElse(fixedMetric(name))
  def contains(name: String): Boolean = get(name).isDefined

  override def toString(): String =
    s"Registry(metrics=${countersRef.get().size}, fixedMetrics=${fixedCountersRef.get().size})"
}

object Registry {
  val global: Registry = Registry()
  def apply(): Registry = new Registry()
}
