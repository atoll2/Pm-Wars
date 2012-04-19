package adwap.android.project

import java.net._
import java.io._
import scala.actors._

class Client {
  val socket = new Socket(InetAddress.getLocalHost(),1914);	
  println("Demande de connexion")
}

class Server(var nbremo:Int = 0) {
  var listre:List[Remote]=Nil
  val socket =  new ServerSocket(1914)
  val local = InetAddress.getLocalHost()
  println("L'adresse locale est:" +local)
  while(nbremo>0){
    nbremo -= 1
    println("about to block")
    val clientSocket = socket.accept()
    val sc = new ServerClient(None,clientSocket)
    val remote = Remote("Remote"+nbremo,0,nbremo+10,false,sc)
    listre ::= remote
    sc.remote=Some(remote)
    sc.start

  }
  println("Nombre client suffisant")
}



class ServerClient(var remote:Option[Remote],socket:Socket) extends Actor {

def act() {
  loop {
   receive {
    case y:Action => ()
    case _ => println("lol")
   }
  }
}
}
