package com.scalarometer.statistics

import java.io.Serializable
import scala.collection.mutable

@SerialVersionUID(20261126010000L)
abstract class Gauge(val name: String) extends Metric with Serializable with StatisticsLogging {
  override def toString(): String = s"Gauge($name)"
}

object Gauge {
  @FunctionalInterface
  trait LongSampler extends Serializable {
    def sample(): Long
  }

  @FunctionalInterface
  trait DoubleSampler extends Serializable {
    def sample(): Double
  }

  @FunctionalInterface
  trait IntSampler extends Serializable {
    def sample(): Int
  }

  @SerialVersionUID(20261126010000L)
  private final class MultiGauge(name: String, f: (String, Boolean) => Map[String, Number]) extends Gauge(name) {
    override def data(reset: Boolean = false): Map[String, Number] = f(name, reset)

    override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
      val _ = builder ++= f(name, reset)
    }
  }

  @SerialVersionUID(20261126010000L)
  private final class SingleGauge(name: String, f: => Number) extends Gauge(name) {
    override def data(reset: Boolean = false): Map[String, Number] =
      new scala.collection.immutable.Map.Map1(name, f)

    override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
      val _ = builder.addOne((name, f))
    }
  }

  @SerialVersionUID(20261126010000L)
  private final class LongGauge(name: String, sampler: LongSampler) extends Gauge(name) {
    override def data(reset: Boolean = false): Map[String, Number] =
      new scala.collection.immutable.Map.Map1(name, java.lang.Long.valueOf(sampler.sample()))

    override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
      val _ = builder.addOne((name, java.lang.Long.valueOf(sampler.sample())))
    }
  }

  @SerialVersionUID(20261126010000L)
  private final class DoubleGauge(name: String, sampler: DoubleSampler) extends Gauge(name) {
    override def data(reset: Boolean = false): Map[String, Number] =
      new scala.collection.immutable.Map.Map1(name, java.lang.Double.valueOf(sampler.sample()))

    override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
      val _ = builder.addOne((name, java.lang.Double.valueOf(sampler.sample())))
    }
  }

  @SerialVersionUID(20261126010000L)
  private final class IntGauge(name: String, sampler: IntSampler) extends Gauge(name) {
    override def data(reset: Boolean = false): Map[String, Number] =
      new scala.collection.immutable.Map.Map1(name, java.lang.Integer.valueOf(sampler.sample()))

    override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
      val _ = builder.addOne((name, java.lang.Integer.valueOf(sampler.sample())))
    }
  }

  def apply(name: String, f: (String, Boolean) => Map[String, Number]): Gauge =
    new MultiGauge(name, f)

  def single(name: String)(f: => Number): Gauge =
    new SingleGauge(name, f)

  def fromLong(name: String, sampler: LongSampler): Gauge =
    new LongGauge(name, sampler)

  inline def long(name: String)(inline f: Long): Gauge =
    fromLong(name, () => f)

  def fromDouble(name: String, sampler: DoubleSampler): Gauge =
    new DoubleGauge(name, sampler)

  inline def double(name: String)(inline f: Double): Gauge =
    fromDouble(name, () => f)

  def fromInt(name: String, sampler: IntSampler): Gauge =
    new IntGauge(name, sampler)

  inline def int(name: String)(inline f: Int): Gauge =
    fromInt(name, () => f)
}
