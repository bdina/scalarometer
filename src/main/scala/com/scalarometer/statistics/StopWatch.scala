package com.statistics

class StopWatch() {
  import StopWatch._

  private var _state: State = State.STOPPED

  private var t0 = 0L
  private var t1 = 0L

  def start(): Unit = _state match {
    case State.STOPPED =>
      t0 = System.nanoTime()
      _state = State.RUNNING
    case _ =>
      ()
  }

  def mark(): Marker = _state match {
    case State.STOPPED =>
      Marker.fromNanoTime(t1)
    case _ =>
      Marker.now
  }

  def stop(): Long = {
    _state match {
      case State.RUNNING =>
        t1 = System.nanoTime()
        _state = State.STOPPED
      case _ =>
        ()
    }
    elapsed
  }

  def state: State = this._state

  def elapsed: Long = if (_state == State.STOPPED) (t1 - t0) / 1000000L else (System.nanoTime() - t0) / 1000000L

  override def toString(): String = s"StopWatch [ state: ${_state} , t0: $t0 , t1: $t1 , elapsed: $elapsed ]"
}
object StopWatch {
  sealed trait State
  object State {
    case object STOPPED extends State
    case object RUNNING extends State
  }

  case class Marker(timestamp: Long)
  object Marker {
    def now: Marker = Marker(System.nanoTime())
    def fromNanoTime(nanotime: Long): Marker = Marker(nanotime)
  }

  def now: Long = System.nanoTime()

  def elapsed(t0: Long): Long = (System.nanoTime() - t0) / 1000000L
  def elapsed(t0: Marker): Long = elapsed(t0.timestamp)

  def apply() = new StopWatch()
}
