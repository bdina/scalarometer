package com.scalarometer.statistics

import java.util.logging.Level
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StatisticsLoggingSpec extends AnyFlatSpec with Matchers {

  private class ProbeLogger extends StatisticsLogging {
    def emitFine(msg: => String): Unit = logFine(msg)
    def emitFinest(msg: => String): Unit = logFinest(msg)
    def emitInfo(msg: => String): Unit = logInfo(msg)
  }

  "StatisticsLogging" should "cache per-class Logger instances without per-instance fields" in {
    val c1 = Counter("a")
    val c2 = Counter("b")
    val h = Histogram("h")

    c1.logger should be theSameInstanceAs c2.logger
    c1.logger.getName shouldBe classOf[Counter].getName
    h.logger.getName shouldBe classOf[Histogram].getName
    c1.logger should not be theSameInstanceAs(h.logger)

    val declaredFields = classOf[Counter].getDeclaredFields.map(_.getName)
    declaredFields.exists(_.contains("logger")) shouldBe false
  }

  it should "avoid evaluating log messages when log level is disabled" in {
    val probe = new ProbeLogger
    val previousLevel = probe.logger.getLevel
    val previousUseParent = probe.logger.getUseParentHandlers
    var evaluations = 0

    try {
      probe.logger.setUseParentHandlers(false)
      probe.logger.setLevel(Level.WARNING)

      probe.emitFine {
        evaluations += 1
        "fine-msg"
      }
      probe.emitFinest {
        evaluations += 1
        "finest-msg"
      }
      probe.emitInfo {
        evaluations += 1
        "info-msg"
      }
      evaluations shouldBe 0

      probe.logger.setLevel(Level.FINEST)
      probe.emitFine {
        evaluations += 1
        "fine-msg"
      }
      probe.emitFinest {
        evaluations += 1
        "finest-msg"
      }
      probe.emitInfo {
        evaluations += 1
        "info-msg"
      }
      evaluations shouldBe 3
    } finally {
      probe.logger.setLevel(previousLevel)
      probe.logger.setUseParentHandlers(previousUseParent)
    }
  }
}
