package clicker.server

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import clicker.{BuyEquipment, Click, GameState, Update, UpdateGames}
import clicker.game.GameActor
import com.corundumstudio.socketio.listener.{ConnectListener, DataListener, DisconnectListener}
import com.corundumstudio.socketio.{AckRequest, Configuration, SocketIOClient, SocketIOServer}
import scala.io.Source

class ConnectionListener() extends ConnectListener {
  override def onConnect(client: SocketIOClient): Unit = {println("Connected: " + client)}
}
class DisconnectionListener(server: ClickerServer) extends DisconnectListener {
  override def onDisconnect(socket: SocketIOClient): Unit = {
    if(server.userList.contains(socket)) {
      val username = server.userList(socket)
      val actor = server.socketToActor(socket)
      server.userList -= socket
      server.clientList -= username
      server.socketToActor -=socket
      server.userToActor -= username
      server.actorToSocket -= actor

    }
  }
}

class startListener(server:ClickerServer,config:String, system:ActorSystem)extends DataListener[String]{
  override def onData(socket: SocketIOClient, username: String, ackRequest: AckRequest): Unit = {
      val newActorRef = system.actorOf(Props(classOf[GameActor], username, config))
      socket.sendEvent("initialize", config)
      server.userList += socket -> username
      server.clientList += username -> socket
      server.userToActor += (username -> newActorRef)
      server.socketToActor += (socket -> newActorRef)
     server.actorToSocket += newActorRef -> socket

  }
}
class clickListener(server:ClickerServer)extends DataListener[Nothing]{
  override def onData(socket: SocketIOClient, noMessage: Nothing, ackRequest: AckRequest): Unit = {
    if(server.socketToActor.contains(socket)){
      server.socketToActor(socket) ! Click
    }
  }}

class buyListener(server:ClickerServer)extends DataListener[String]{
  override def onData(socket: SocketIOClient, equipID: String, ackRequest: AckRequest): Unit = {
    if(server.socketToActor.contains(socket)) {
      server.socketToActor(socket) ! BuyEquipment(equipID)
    }
  }}

class ClickerServer(val configuration: String) extends Actor {
  val system = ActorSystem("GameActors")
  var clientList:Map[String, SocketIOClient]=Map()
  var userList:Map[SocketIOClient, String]=Map()
  var userToActor:Map[String,ActorRef]=Map()
  var socketToActor:Map[SocketIOClient, ActorRef]=Map()
  var actorToSocket:Map[ActorRef, SocketIOClient]= Map()
  val config: Configuration = new Configuration {
    setHostname("localhost")
    setPort(8080)
  }
  val server: SocketIOServer = new SocketIOServer(config)
  server.addConnectListener(new ConnectionListener())
  server.addDisconnectListener(new DisconnectionListener(this))
  server.addEventListener("startGame", classOf[String], new startListener(this, configuration, system))
  server.addEventListener("click", classOf[Nothing], new clickListener(this))
  server.addEventListener("buy", classOf[String], new buyListener(this))
  server.start()


  override def receive: Receive = {
    case UpdateGames =>
      socketToActor.values.toList.foreach(_ ! Update)

    case g:GameState =>
      if(actorToSocket.contains(sender())){
        val sendSocket:SocketIOClient =actorToSocket(sender())
          sendSocket.sendEvent("gameState",g.gameState)
      }

  }


  override def postStop(): Unit = {
    println("stopping server")
    server.stop()
  }
}

object ClickerServer {

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem()
    import actorSystem.dispatcher

    import scala.concurrent.duration._

    val configuration: String = Source.fromFile("codeConfig.json").mkString

    val server = actorSystem.actorOf(Props(classOf[ClickerServer], configuration))

    actorSystem.scheduler.schedule(0.milliseconds, 100.milliseconds, server, UpdateGames)
  }
}
