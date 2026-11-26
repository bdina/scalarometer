package com.scalarometer.statistics

import java.util.{Arrays, Comparator}

trait MetricsFormat {
  def contentType: String

  def writeTo(registry: Registry, out: Appendable, reset: Boolean = false): Unit
  def writeMetrics(metrics: Iterable[Metric], out: Appendable, reset: Boolean = false): Unit
  def writeStats(stats: Map[String, Number], out: Appendable): Unit

  def format(registry: Registry, reset: Boolean = false): String = {
    val sb = new java.lang.StringBuilder(256)
    writeTo(registry, sb, reset)
    sb.toString
  }

  def formatMetrics(metrics: Iterable[Metric], reset: Boolean = false): String = {
    val sb = new java.lang.StringBuilder(256)
    writeMetrics(metrics, sb, reset)
    sb.toString
  }

  def formatStats(stats: Map[String, Number]): String = {
    val sb = new java.lang.StringBuilder(256)
    writeStats(stats, sb)
    sb.toString
  }
}

object MetricsFormat {
  private val HexDigits: String = "0123456789abcdef"
  private val EmptyEntries: Array[(String, Number)] = new Array[(String, Number)](0)

  private val EntryKeyComparator: Comparator[(String, Number)] =
    (a, b) => a._1.compareTo(b._1)

  private def collectSorted(registry: Registry, reset: Boolean): Array[(String, Number)] = {
    val resettable = registry.metrics
    val fixed = registry.fixedMetrics
    if (resettable.isEmpty && fixed.isEmpty) {
      EmptyEntries
    } else {
      val builder = Array.newBuilder[(String, Number)]
      builder.sizeHint((resettable.size + fixed.size) * 2)
      val it1 = resettable.iterator
      while (it1.hasNext) {
        it1.next().appendData(builder, reset)
      }
      val it2 = fixed.iterator
      while (it2.hasNext) {
        it2.next().appendData(builder, reset = false)
      }
      val arr = builder.result()
      if (arr.length > 1) Arrays.sort(arr, EntryKeyComparator)
      arr
    }
  }

  private def collectSorted(metrics: Iterable[Metric], reset: Boolean): Array[(String, Number)] = {
    if (metrics.isEmpty) {
      EmptyEntries
    } else {
      val hint = metrics.knownSize
      val builder = Array.newBuilder[(String, Number)]
      builder.sizeHint(if (hint > 0) hint * 2 else 16)
      val it = metrics.iterator
      while (it.hasNext) {
        it.next().appendData(builder, reset)
      }
      val arr = builder.result()
      if (arr.length > 1) Arrays.sort(arr, EntryKeyComparator)
      arr
    }
  }

  private def collectSorted(stats: Map[String, Number]): Array[(String, Number)] = {
    if (stats.isEmpty) {
      EmptyEntries
    } else {
      val arr = new Array[(String, Number)](stats.size)
      val it = stats.iterator
      var i = 0
      while (it.hasNext) {
        arr(i) = it.next()
        i += 1
      }
      if (arr.length > 1) Arrays.sort(arr, EntryKeyComparator)
      arr
    }
  }

  private def appendLogfmtKey(out: Appendable, key: String): Unit = {
    val len = key.length
    var start = 0
    var i = 0
    while (i < len) {
      val c = key.charAt(i)
      if (c <= ' ' || c == '=' || c == '"' || c == '\\') {
        if (i > start) {
          val _ = out.append(key, start, i)
        }
        val _ = out.append('_')
        start = i + 1
      }
      i += 1
    }
    if (start < len) {
      val _ = out.append(key, start, len)
    }
  }

  private def appendLogfmtNumber(out: Appendable, value: Number): Unit = value match {
    case l: java.lang.Long =>
      out match {
        case sb: java.lang.StringBuilder => val _ = sb.append(l.longValue())
        case _                           => val _ = out.append(java.lang.Long.toString(l.longValue()))
      }
    case i: java.lang.Integer =>
      out match {
        case sb: java.lang.StringBuilder => val _ = sb.append(i.intValue())
        case _                           => val _ = out.append(java.lang.Integer.toString(i.intValue()))
      }
    case d: java.lang.Double =>
      val v = d.doubleValue()
      if (java.lang.Double.isNaN(v)) {
        val _ = out.append("NaN")
      } else if (v == Double.PositiveInfinity) {
        val _ = out.append("+Inf")
      } else if (v == Double.NegativeInfinity) {
        val _ = out.append("-Inf")
      } else {
        out match {
          case sb: java.lang.StringBuilder => val _ = sb.append(v)
          case _                           => val _ = out.append(java.lang.Double.toString(v))
        }
      }
    case f: java.lang.Float =>
      val v = f.floatValue()
      if (java.lang.Float.isNaN(v)) {
        val _ = out.append("NaN")
      } else if (v == Float.PositiveInfinity) {
        val _ = out.append("+Inf")
      } else if (v == Float.NegativeInfinity) {
        val _ = out.append("-Inf")
      } else {
        out match {
          case sb: java.lang.StringBuilder => val _ = sb.append(v)
          case _                           => val _ = out.append(java.lang.Float.toString(v))
        }
      }
    case s: java.lang.Short =>
      out match {
        case sb: java.lang.StringBuilder => val _ = sb.append(s.intValue())
        case _                           => val _ = out.append(java.lang.Integer.toString(s.intValue()))
      }
    case b: java.lang.Byte =>
      out match {
        case sb: java.lang.StringBuilder => val _ = sb.append(b.intValue())
        case _                           => val _ = out.append(java.lang.Integer.toString(b.intValue()))
      }
    case other =>
      val _ = out.append(other.toString)
  }

  private def appendJsonString(out: Appendable, s: String): Unit = {
    val _ = out.append('"')
    val len = s.length
    var start = 0
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c == '"' || c == '\\' || c < 0x20) {
        if (i > start) {
          val _ = out.append(s, start, i)
        }
        c match {
          case '"'  => val _ = out.append("\\\"")
          case '\\' => val _ = out.append("\\\\")
          case '\b' => val _ = out.append("\\b")
          case '\f' => val _ = out.append("\\f")
          case '\n' => val _ = out.append("\\n")
          case '\r' => val _ = out.append("\\r")
          case '\t' => val _ = out.append("\\t")
          case ctrl =>
            val _ = out.append("\\u00")
            val _ = out.append(HexDigits.charAt((ctrl >> 4) & 0xf))
            val _ = out.append(HexDigits.charAt(ctrl & 0xf))
        }
        start = i + 1
      }
      i += 1
    }
    if (start < len) {
      val _ = out.append(s, start, len)
    }
    val _ = out.append('"')
  }

  private def appendJsonNumber(out: Appendable, value: Number): Unit = value match {
    case l: java.lang.Long =>
      out match {
        case sb: java.lang.StringBuilder => val _ = sb.append(l.longValue())
        case _                           => val _ = out.append(java.lang.Long.toString(l.longValue()))
      }
    case i: java.lang.Integer =>
      out match {
        case sb: java.lang.StringBuilder => val _ = sb.append(i.intValue())
        case _                           => val _ = out.append(java.lang.Integer.toString(i.intValue()))
      }
    case d: java.lang.Double =>
      val v = d.doubleValue()
      if (java.lang.Double.isNaN(v) || java.lang.Double.isInfinite(v)) {
        val _ = out.append("null")
      } else {
        out match {
          case sb: java.lang.StringBuilder => val _ = sb.append(v)
          case _                           => val _ = out.append(java.lang.Double.toString(v))
        }
      }
    case f: java.lang.Float =>
      val v = f.floatValue()
      if (java.lang.Float.isNaN(v) || java.lang.Float.isInfinite(v)) {
        val _ = out.append("null")
      } else {
        out match {
          case sb: java.lang.StringBuilder => val _ = sb.append(v)
          case _                           => val _ = out.append(java.lang.Float.toString(v))
        }
      }
    case s: java.lang.Short =>
      out match {
        case sb: java.lang.StringBuilder => val _ = sb.append(s.intValue())
        case _                           => val _ = out.append(java.lang.Integer.toString(s.intValue()))
      }
    case b: java.lang.Byte =>
      out match {
        case sb: java.lang.StringBuilder => val _ = sb.append(b.intValue())
        case _                           => val _ = out.append(java.lang.Integer.toString(b.intValue()))
      }
    case other =>
      val _ = out.append(other.toString)
  }

  private def writeLogfmtEntries(entries: Array[(String, Number)], out: Appendable): Unit = {
    val len = entries.length
    var written = 0
    var i = 0
    while (i < len) {
      val entry = entries(i)
      if (i + 1 >= len || entry._1 != entries(i + 1)._1) {
        if (written > 0) {
          val _ = out.append(' ')
        }
        appendLogfmtKey(out, entry._1)
        val _ = out.append('=')
        appendLogfmtNumber(out, entry._2)
        written += 1
      }
      i += 1
    }
  }

  private def writeJsonEntries(entries: Array[(String, Number)], out: Appendable): Unit = {
    val _ = out.append('{')
    val len = entries.length
    var written = 0
    var i = 0
    while (i < len) {
      val entry = entries(i)
      if (i + 1 >= len || entry._1 != entries(i + 1)._1) {
        if (written > 0) {
          val _ = out.append(',')
        }
        appendJsonString(out, entry._1)
        val _ = out.append(':')
        appendJsonNumber(out, entry._2)
        written += 1
      }
      i += 1
    }
    val _ = out.append('}')
  }

  object Logfmt extends MetricsFormat {
    override val contentType: String = "text/plain; charset=utf-8"

    override def writeTo(registry: Registry, out: Appendable, reset: Boolean = false): Unit =
      writeLogfmtEntries(collectSorted(registry, reset), out)

    override def writeMetrics(metrics: Iterable[Metric], out: Appendable, reset: Boolean = false): Unit =
      writeLogfmtEntries(collectSorted(metrics, reset), out)

    override def writeStats(stats: Map[String, Number], out: Appendable): Unit =
      writeLogfmtEntries(collectSorted(stats), out)
  }

  object Json extends MetricsFormat {
    override val contentType: String = "application/json; charset=utf-8"

    override def writeTo(registry: Registry, out: Appendable, reset: Boolean = false): Unit =
      writeJsonEntries(collectSorted(registry, reset), out)

    override def writeMetrics(metrics: Iterable[Metric], out: Appendable, reset: Boolean = false): Unit =
      writeJsonEntries(collectSorted(metrics, reset), out)

    override def writeStats(stats: Map[String, Number], out: Appendable): Unit =
      writeJsonEntries(collectSorted(stats), out)
  }
}
