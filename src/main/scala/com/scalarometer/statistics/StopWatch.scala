package com.scalarometer.statistics

import java.util.concurrent.atomic.AtomicReference

class StopWatch(clock: () => Long = () => System.nanoTime()) {
  import StopWatch._

  private case class TimingState(state: State, t0: Long, t1: Long)

  private val stateRef: AtomicReference[TimingState] =
    new AtomicReference(TimingState(State.STOPPED, 0L, 0L))

  def start(): Unit = {
    val _ = stateRef.updateAndGet { current =>
      current.state match {
        case State.STOPPED => TimingState(State.RUNNING, clock(), 0L)
        case State.RUNNING => current
      }
    }
  }

  def mark(): Marker = {
    val current = stateRef.get()
    current.state match {
      case State.STOPPED => Marker.fromNanoTime(current.t1)
      case State.RUNNING => Marker.fromNanoTime(clock())
    }
  }

  def stop(): Long = {
    val updated = stateRef.updateAndGet { current =>
      current.state match {
        case State.RUNNING => TimingState(State.STOPPED, current.t0, clock())
        case State.STOPPED => current
      }
    }
    (updated.t1 - updated.t0) / NanosPerMilli
  }

  def reset(): Unit = stateRef.set(TimingState(State.STOPPED, 0L, 0L))

  def state: State = stateRef.get().state
  def isRunning: Boolean = state == State.RUNNING

  def elapsedNanos: Long = {
    val current = stateRef.get()
    current.state match {
      case State.STOPPED => current.t1 - current.t0
      case State.RUNNING => clock() - current.t0
    }
  }

  def elapsed: Long = elapsedNanos / NanosPerMilli

  override def toString(): String = {
    val current = stateRef.get()
    val elapsedMs = (current.state match {
      case State.STOPPED => current.t1 - current.t0
      case State.RUNNING => clock() - current.t0
    }) / NanosPerMilli
    s"StopWatch [ state: ${current.state} , t0: ${current.t0} , t1: ${current.t1} , elapsed: $elapsedMs ]"
  }
}

object StopWatch {
  private val NanosPerMilli: Long = 1_000_000L

  enum State {
    case STOPPED, RUNNING
  }

  final case class Marker(timestamp: Long)
  object Marker {
    def now: Marker = Marker(System.nanoTime())
    def fromNanoTime(nanotime: Long): Marker = Marker(nanotime)
  }

  def now: Long = System.nanoTime()

  def elapsed(t0: Long): Long = (System.nanoTime() - t0) / NanosPerMilli
  def elapsed(t0: Marker): Long = elapsed(t0.timestamp)

  def timed[A](block: => A): (A, Long) = {
    val sw = StopWatch()
    sw.start()
    val result = block
    val ms = sw.stop()
    (result, ms)
  }

  def apply(): StopWatch = new StopWatch()
  def apply(clock: () => Long): StopWatch = new StopWatch(clock)
}
