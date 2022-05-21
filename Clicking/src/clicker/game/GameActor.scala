package clicker.game

import akka.actor.Actor
import clicker.{BuyEquipment, Click, GameState, Update}
import play.api.libs.json.{JsValue, Json}

class GameActor(username: String, configuration: String) extends Actor {
  var gold: Double = 0.0
  var increment: Double = 1.0
  var idleInc: Double = 0.0
  var currentTime: Long = System.nanoTime()
  var lastTime: Long = 0
  var equipmentIds: List[String] = List()
  val config:JsValue = Json.parse(configuration)
  val equipList:List[JsValue] = (config \ "equipment").as[List[JsValue]]
  def getIDS(list: List[JsValue]):Map[String, Double] ={
    var idList:Map[String, Double]=Map()
    for(i<-list){
      val id:String=(i \ "id").as[String]
      val cost: Double = (i \ "initialCost").as[Double]
      idList += id -> cost
    }
    idList}
  var IDandCost: Map[String, Double] = getIDS(equipList)
  var originalIdCost: Map[String, Double] = getIDS(equipList)
  override def receive: Receive = {

    case Click =>
      gold = gold + increment

    case Update =>
      def countEquipment(e: List[String], equipName: String):JsValue = {
          var count = 0
        for (i <- e) {
          if (i == equipName) {
            count = count + 1
          }
        }
        Json.toJson(count)}
      lastTime = currentTime
      currentTime = System.nanoTime()
      gold = gold + (((currentTime-lastTime)/ 1000000000.0) * idleInc)
      var sendEquip: List[JsValue] = List()
      for((i,c)<-IDandCost){
        val count = countEquipment(equipmentIds, i)
        val infoMap:Map[String, JsValue]=Map("id" -> Json.toJson(i), "numberOwned"->Json.toJson(count), "cost"->Json.toJson(c))
        val send:JsValue=Json.toJson(infoMap)
        sendEquip ::= send
      }
      val sendMap: Map[String, JsValue] = Map("username" -> Json.toJson(username), "currency" -> Json.toJson(gold), "equipment" -> Json.toJson(sendEquip))
      val sendAll = Json.toJson(sendMap)
      val jsonString:String = Json.stringify(sendAll)
      sender() ! GameState(jsonString)

    case buy: BuyEquipment =>
      def countList(e: List[String], equipName: String):Int = {
        var count = 0
        for (i <- e) {
          if (i == equipName) {
            count = count + 1
          }
        }
        count}
      val parsed:JsValue = Json.parse(configuration)
      val equipmentList:List[JsValue] = (parsed \ "equipment").as[List[JsValue]]
      var idAndEquip: Map[String, JsValue] = Map()
      for(equipment<-equipmentList){
        val equipId:String=(equipment \ "id").as[String]
        idAndEquip += equipId  -> equipment
      }
      if(idAndEquip.contains(buy.equipmentId)){
        val cost:Double =(idAndEquip(buy.equipmentId) \ "initialCost").as[Double]
        val moneyIdle:Double =(idAndEquip(buy.equipmentId) \ "incomePerSecond").as[Double]
        val moneyClick:Double =(idAndEquip(buy.equipmentId) \ "incomePerClick").as[Double]
        val priceExponent:Double =(idAndEquip(buy.equipmentId)\ "priceExponent").as[Double]
        var increasedCost = 0.0
        if(countList(equipmentIds, buy.equipmentId)==0) {
          increasedCost= cost
        }else{
          increasedCost = Math.pow(priceExponent,countList(equipmentIds, buy.equipmentId))*originalIdCost(buy.equipmentId)
        }
        if(gold>=increasedCost){
          gold=gold-IDandCost(buy.equipmentId)
          idleInc=idleInc+moneyIdle
          increment =increment + moneyClick
          equipmentIds ::= buy.equipmentId
          increasedCost = Math.pow(priceExponent,countList(equipmentIds, buy.equipmentId))*originalIdCost(buy.equipmentId)
          IDandCost += buy.equipmentId -> increasedCost
        }
        }
      }
  }

