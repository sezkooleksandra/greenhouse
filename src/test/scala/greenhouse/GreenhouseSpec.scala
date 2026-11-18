package greenhouse

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GreenhouseSpec extends AnyFunSuite with Matchers {

  private val cfg = TankConfig(capacity = 1000.0, minLevel = 100.0)

  test("повний сценарій: наповнення -> полив -> добрива -> злив -> підсумки") {
    val events = List(
      Refill(800),
      Irrigate("s1", 100, 20),      
      AddFertilizer(10),
      Irrigate("s2", 150, 10),      
      Irrigate("s3", 60, 20),        
      DrainSludge(40)
    )
    val result = Greenhouse.process(cfg, events)
    result.isRight shouldBe true
    val st = result.toOption.get

    Greenhouse.currentLevel(st) shouldBe 450.0
    Greenhouse.totalIrrigated(st) shouldBe 310.0
    st.fertilizerLiters shouldBe 10.0

    val top2 = Greenhouse.topIntensive(st, 2).map(_.id)
    top2 shouldBe List("s2", "s1")
  }

  test("граничний випадок: полив заборонений нижче мінімального датчика") {
    val events = List(
      Refill(300),
      Irrigate("s1", 250, 10),  
      Irrigate("s2", 10, 5)      
    )
    Greenhouse.process(cfg, events) shouldBe Left(BelowMinSensor(50.0, 100.0))
  }

  test("не можна переповнити бак") {
    Greenhouse.process(cfg, List(Refill(900), Refill(200))) shouldBe
      Left(TankOverflow(200.0, 100.0))
  }

  test("не можна полити більше, ніж є води") {
    Greenhouse.process(cfg, List(Refill(150), Irrigate("s1", 200, 10))) shouldBe
      Left(InsufficientWater(200.0, 150.0))
  }

  test("не можна злити осад більше, ніж є води") {
    Greenhouse.process(cfg, List(Refill(50), DrainSludge(80))) shouldBe
      Left(InsufficientWater(80.0, 50.0))
  }

  test("невалідні кількості відхиляються") {
    Greenhouse.process(cfg, List(Refill(-5))) shouldBe Left(InvalidAmount(-5.0))
    Greenhouse.process(cfg, List(Refill(100), Irrigate("s1", 10, 0))) shouldBe Left(InvalidAmount(0.0))
  }

  test("дублікат ідентифікатора сеансу відхиляється") {
    val events = List(Refill(500), Irrigate("s1", 10, 5), Irrigate("s1", 10, 5))
    Greenhouse.process(cfg, events) shouldBe Left(DuplicateSession("s1"))
  }

  test("топ-2 при єдиному сеансі повертає один елемент") {
    val st = Greenhouse.process(cfg, List(Refill(500), Irrigate("only", 50, 10))).toOption.get
    Greenhouse.topIntensive(st, 2).map(_.id) shouldBe List("only")
  }
}
