package com.statistics

case class Registry() extends Statistics {
  protected var counters: Set[Metric] = Set.empty
  protected var fixed_counters: Set[Metric] = Set.empty

  def stats(reset: Boolean): Map[String,Number] = (
    counters.flatMap { case counter =>
      counter.data(reset).map { case (name, counts) => name -> counts }
    }
    ++
    fixed_counters.flatMap { case counter =>
      counter.data(reset=false).map { case (name, counts) => name -> counts }
    }
  ).toMap[String,Number]

  def registerCounter(counter: Counter) = counters + counter
  def registerFixedCounter(counter: Counter) = fixed_counters + counter

  def registerMetric(metric: Metric) = counters + metric
  def unregisterMetric(metric: Metric) = counters - metric

  def registerFixedMetric(metric: Metric) = fixed_counters + metric
  def unregisterFixedMetric(metric: Metric) = fixed_counters - metric

  def metric(name: String): Option[Metric] = counters.find { case metric => metric.name == name }
  def fixedMetric(name: String): Option[Metric] = fixed_counters.find { case metric => metric.name == name }
}
object Registry {
  val global = Registry()
}
