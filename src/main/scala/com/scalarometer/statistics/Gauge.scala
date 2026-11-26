package com.scalarometer.statistics

import java.io.Serializable
import scala.collection.mutable

@SerialVersionUID(20261126010000L)
abstract class Gauge(val name: String) extends Metric with Serializable with StatisticsLogging {
  override def toString(): String = s"Gauge($name)"
}

object Gauge {
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
  private final class LongGauge(name: String, f: => Long) extends Gauge(name) {
    override def data(reset: Boolean = false): Map[String, Number] =
      new scala.collection.immutable.Map.Map1(name, java.lang.Long.valueOf(f))

    override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
      val _ = builder.addOne((name, java.lang.Long.valueOf(f)))
    }
  }

  @SerialVersionUID(20261126010000L)
  private final class DoubleGauge(name: String, f: => Double) extends Gauge(name) {
    override def data(reset: Boolean = false): Map[String, Number] =
      new scala.collection.immutable.Map.Map1(name, java.lang.Double.valueOf(f))

    override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
      val _ = builder.addOne((name, java.lang.Double.valueOf(f)))
    }
  }

  @SerialVersionUID(20261126010000L)
  private final class IntGauge(name: String, f: => Int) extends Gauge(name) {
    override def data(reset: Boolean = false): Map[String, Number] =
      new scala.collection.immutable.Map.Map1(name, java.lang.Integer.valueOf(f))

    override def appendData(builder: mutable.Builder[(String, Number), ?], reset: Boolean = false): Unit = {
      val _ = builder.addOne((name, java.lang.Integer.valueOf(f)))
    }
  }

  def apply(name: String, f: (String, Boolean) => Map[String, Number]): Gauge =
    new MultiGauge(name, f)

  def single(name: String)(f: => Number): Gauge =
    new SingleGauge(name, f)

  def long(name: String)(f: => Long): Gauge =
    new LongGauge(name, f)

  def double(name: String)(f: => Double): Gauge =
    new DoubleGauge(name, f)

  def int(name: String)(f: => Int): Gauge =
    new IntGauge(name, f)
}
