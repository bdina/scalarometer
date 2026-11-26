package com.scalarometer.statistics

import java.util.concurrent.atomic.AtomicReference
import java.util.function.LongSupplier
import scala.annotation.{tailrec, targetName}

open class StopWatch(clock: LongSupplier = StopWatch.SystemClock) {
  import StopWatch._

  private val stateRef: AtomicReference[TimingState] =
    new AtomicReference(InitialState)

  private inline def readClock(): Long =
    if (clock eq SystemClock) System.nanoTime() else clock.getAsLong()

  def start(): Unit = {
    @tailrec
    def loop(): Unit = {
      val current = stateRef.get()
      current.state match {
        case State.STOPPED =>
          val next = new TimingState(State.RUNNING, readClock(), 0L)
          if (!stateRef.compareAndSet(current, next)) loop()
        case State.RUNNING => ()
      }
    }
    loop()
  }

  def mark(): Marker = {
    val current = stateRef.get()
    current.state match {
      case State.STOPPED => Marker.fromNanoTime(current.t1)
      case State.RUNNING => Marker.fromNanoTime(readClock())
    }
  }

  def stop(): Long = {
    @tailrec
    def loop(): TimingState = {
      val current = stateRef.get()
      current.state match {
        case State.RUNNING =>
          val next = new TimingState(State.STOPPED, current.t0, readClock())
          if (stateRef.compareAndSet(current, next)) next
          else loop()
        case State.STOPPED => current
      }
    }
    val updated = loop()
    (updated.t1 - updated.t0) / NanosPerMilli
  }

  def reset(): Unit = stateRef.set(InitialState)

  def state: State = stateRef.get().state
  def isRunning: Boolean = state == State.RUNNING

  def elapsedNanos: Long = {
    val current = stateRef.get()
    current.state match {
      case State.STOPPED => current.t1 - current.t0
      case State.RUNNING => readClock() - current.t0
    }
  }

  def elapsed: Long = elapsedNanos / NanosPerMilli

  override def toString(): String = {
    val current = stateRef.get()
    val elapsedMs = (current.state match {
      case State.STOPPED => current.t1 - current.t0
      case State.RUNNING => readClock() - current.t0
    }) / NanosPerMilli
    s"StopWatch [ state: ${current.state} , t0: ${current.t0} , t1: ${current.t1} , elapsed: $elapsedMs ]"
  }
}

object StopWatch {
  inline val NanosPerMilli = 1_000_000L

  private val SystemClock: LongSupplier = () => System.nanoTime()

  private final class TimingState(val state: State, val t0: Long, val t1: Long)
  private val InitialState: TimingState = new TimingState(State.STOPPED, 0L, 0L)

  enum State derives CanEqual {
    case STOPPED, RUNNING
  }

  opaque type Marker = Long

  object Marker {
    given CanEqual[Marker, Marker] = CanEqual.derived

    inline def apply(timestamp: Long): Marker = timestamp
    inline def now: Marker = System.nanoTime()
    inline def fromNanoTime(nanotime: Long): Marker = nanotime

    extension (marker: Marker) {
      inline def timestamp: Long = marker
      inline def elapsedNanos: Long = System.nanoTime() - marker
      inline def elapsed: Long = (System.nanoTime() - marker) / NanosPerMilli
    }
  }

  def now: Long = System.nanoTime()

  def elapsed(t0: Long): Long = (System.nanoTime() - t0) / NanosPerMilli

  @targetName("elapsedFromMarker")
  inline def elapsed(t0: Marker): Long = (System.nanoTime() - Marker.timestamp(t0)) / NanosPerMilli

  inline def timed[A](inline block: A): (A, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val ms = (System.nanoTime() - t0) / NanosPerMilli
    (result, ms)
  }

  inline def timedNanos[A](inline block: A): (A, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val nanos = System.nanoTime() - t0
    (result, nanos)
  }

  def apply(): StopWatch = new StopWatch()
  def apply(clock: LongSupplier): StopWatch = new StopWatch(clock)
}
