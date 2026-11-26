package com.scalarometer.statistics

import java.time.Duration
import java.util.concurrent.{Executors, ScheduledExecutorService, ScheduledFuture, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.logging.{Level, Logger}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

final class PeriodicReporter private (
    private val registry: Registry,
    private val format: MetricsFormat,
    private val reset: Boolean,
    private val prefix: String,
    private val skipWhenEmpty: Boolean,
    private val flushOnClose: Boolean,
    private val sink: String => Unit,
    private val executor: ScheduledExecutorService,
    private val ownsExecutor: Boolean
) extends AutoCloseable
    with StatisticsLogging {

  private val closed: AtomicBoolean = new AtomicBoolean(false)
  private val buffer: java.lang.StringBuilder = new java.lang.StringBuilder(256)
  @volatile private var future: ScheduledFuture[?] | Null = null

  private[statistics] def startSchedule(initialDelayMs: Long, periodMs: Long): Unit = {
    future = executor.scheduleAtFixedRate(
      () => {
        val _ = reportOnce()
      },
      initialDelayMs,
      periodMs,
      TimeUnit.MILLISECONDS
    )
  }

  def isClosed: Boolean = closed.get()

  def reportOnce(): Option[String] = {
    if (skipWhenEmpty && registry.isEmpty) {
      None
    } else {
      val rendered = buffer.synchronized {
        buffer.setLength(0)
        if (prefix.nonEmpty) {
          val _ = buffer.append(prefix)
        }
        val beforeBody = buffer.length()
        format.writeTo(registry, buffer, reset)
        if (skipWhenEmpty && buffer.length() == beforeBody) {
          None
        } else {
          Some(buffer.toString)
        }
      }
      rendered.foreach { payload =>
        try {
          sink(payload)
        } catch {
          case NonFatal(e) =>
            logger.warning(s"PeriodicReporter sink failed: ${e.getMessage}")
        }
      }
      rendered
    }
  }

  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      val scheduled = future
      if (scheduled ne null) {
        val _ = scheduled.cancel(false)
      }
      if (flushOnClose) {
        val _ = reportOnce()
      }
      if (ownsExecutor) {
        executor.shutdown()
      }
      logFine("Stopped PeriodicReporter")
    }
  }
}

object PeriodicReporter {
  private val DefaultThreadName: String = "scalarometer-reporter"

  private def newDaemonExecutor(threadName: String): ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor((r: Runnable) => {
      val t = new Thread(r, threadName)
      t.setDaemon(true)
      t
    })

  private def normalizeMs(duration: Duration): Long =
    math.max(1L, duration.toMillis)

  def start(
      registry: Registry,
      interval: Duration,
      format: MetricsFormat = MetricsFormat.Logfmt,
      reset: Boolean = true,
      prefix: String = "",
      skipWhenEmpty: Boolean = true,
      flushOnClose: Boolean = false,
      initialDelay: Option[Duration] = None,
      executor: Option[ScheduledExecutorService] = None,
      threadName: String = DefaultThreadName
  )(sink: String => Unit): PeriodicReporter = {
    val periodMs = normalizeMs(interval)
    val delayMs = initialDelay.map(d => math.max(0L, d.toMillis)).getOrElse(periodMs)
    val (exec, owns) = executor match {
      case Some(existing) => (existing, false)
      case None           => (newDaemonExecutor(threadName), true)
    }
    val reporter = new PeriodicReporter(
      registry = registry,
      format = format,
      reset = reset,
      prefix = prefix,
      skipWhenEmpty = skipWhenEmpty,
      flushOnClose = flushOnClose,
      sink = sink,
      executor = exec,
      ownsExecutor = owns
    )
    reporter.startSchedule(delayMs, periodMs)
    reporter
  }

  def start(
      registry: Registry,
      interval: FiniteDuration,
      format: MetricsFormat,
      reset: Boolean,
      prefix: String,
      skipWhenEmpty: Boolean,
      flushOnClose: Boolean,
      executor: Option[ScheduledExecutorService]
  )(sink: String => Unit): PeriodicReporter =
    start(
      registry = registry,
      interval = Duration.ofMillis(math.max(1L, interval.toMillis)),
      format = format,
      reset = reset,
      prefix = prefix,
      skipWhenEmpty = skipWhenEmpty,
      flushOnClose = flushOnClose,
      executor = executor
    )(sink)

  def logging(
      registry: Registry,
      interval: Duration = Duration.ofSeconds(60),
      format: MetricsFormat = MetricsFormat.Logfmt,
      reset: Boolean = true,
      prefix: String = "[service:metrics] ",
      targetLogger: Logger = Logger.getLogger(classOf[PeriodicReporter].getName),
      level: Level = Level.INFO,
      skipWhenEmpty: Boolean = true,
      flushOnClose: Boolean = false,
      initialDelay: Option[Duration] = None,
      executor: Option[ScheduledExecutorService] = None,
      threadName: String = DefaultThreadName
  ): PeriodicReporter =
    start(
      registry = registry,
      interval = interval,
      format = format,
      reset = reset,
      prefix = prefix,
      skipWhenEmpty = skipWhenEmpty,
      flushOnClose = flushOnClose,
      initialDelay = initialDelay,
      executor = executor,
      threadName = threadName
    ) { line =>
      if (targetLogger.isLoggable(level)) {
        targetLogger.log(level, line)
      }
    }

  def logging(
      registry: Registry,
      interval: FiniteDuration,
      format: MetricsFormat,
      reset: Boolean,
      prefix: String,
      targetLogger: Logger,
      level: Level
  ): PeriodicReporter =
    logging(
      registry = registry,
      interval = Duration.ofMillis(math.max(1L, interval.toMillis)),
      format = format,
      reset = reset,
      prefix = prefix,
      targetLogger = targetLogger,
      level = level
    )
}
