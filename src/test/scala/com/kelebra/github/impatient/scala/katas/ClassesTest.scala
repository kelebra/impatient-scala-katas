package com.kelebra.github.impatient.scala.katas

import com.kelebra.github.impatient.scala.katas.Classes.{BankingAccount, CycledCounter, ThrowingCounter, Time24, _}
import org.scalatest.{Matchers, WordSpec}

import scala.language.implicitConversions

class ClassesTest extends WordSpec with Matchers {

  "Counter" should {

    "start with 0 in case of overflow" in {
      val counter = CycledCounter
      (0 to Integer.MAX_VALUE).foreach(_ => counter.increment())

      counter.increment()
      counter.current() should be >= 0
    }

    "throw exception in case of overflow" in {
      val counter = ThrowingCounter

      (0 to Integer.MAX_VALUE).foreach(_ => counter.increment())
      an[IllegalStateException] should be thrownBy counter.increment()

    }
  }

  "Banking account" should {

    "maintain big decimal balance" in {
      val account = new BankingAccount[BigDecimal](BigDecimal(0))
      account.deposit(BigDecimal(0.5))
      account.balance shouldBe BigDecimal(0.5)

      account.withdraw(BigDecimal(0.1))
      account.balance shouldBe BigDecimal(0.4)
    }

    "accept conversible payments" in {
      case class Apple(price: Int)
      implicit def appleToBalance(apple: Apple): Int = apple.price

      val account = new BankingAccount[Int](0)

      val expensiveApple = Apple(100)
      val apple = Apple(1)

      account.deposit(expensiveApple)
      account.balance shouldBe expensiveApple.price

      account.withdraw(apple)
      account.balance shouldBe 99
    }
  }

  "Time implementations" should {

    "should be before one another" in {
      val time24 = Time24(12, 50)
      val timeSod = MinutesSinceStartOfDay(25)

      (timeSod < time24) shouldBe true
    }

    "should be after one another" in {
      val time24 = Time24(12, 50)
      val timeSod = MinutesSinceStartOfDay(25)

      (time24 > timeSod) shouldBe true
    }
  }
}
