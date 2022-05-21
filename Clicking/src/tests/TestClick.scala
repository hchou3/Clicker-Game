package tests

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import clicker._
import clicker.game.GameActor
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.duration._
import scala.io.Source


class TestClick extends TestKit(ActorSystem("TestGame"))
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  val EPSILON: Double = 0.000001

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Clicker actor" must {
    "react to clicks and equipment purchases" in {
      val configuration: String = Source.fromFile("goldConfig.json").mkString


      val gameActor = system.actorOf(Props(classOf[GameActor], "test", configuration))
      gameActor ! Click
      gameActor ! Click
      expectNoMessage(200.millis)

      gameActor ! Update
      val state: GameState = expectMsgType[GameState](1000.millis)

      val jsonState = state.gameState
      val gameState: JsValue = Json.parse(jsonState)
      println(gameState)
      val gold = (gameState \ "currency").as[Double]
      val equipment = (gameState \ "equipment").as[List[JsValue]]
      val expectedGold = 2.0
      assert(equalDoubles(gold, expectedGold))

      gameActor ! BuyEquipment("shovel")

      gameActor ! Click

      expectNoMessage(200.millis)

      gameActor ! Update
      val state2: GameState = expectMsgType[GameState](1000.millis)
      val jsonState2 = state2.gameState
      val gameState2: JsValue = Json.parse(jsonState2)
      println(gameState2)
      val gold2 = (gameState2 \ "currency").as[Double]
      val expectedGold2 = 3.0
      assert(equalDoubles(gold2, expectedGold2))
    }
  }
}
