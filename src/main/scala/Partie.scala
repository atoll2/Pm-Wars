package adwap.android.project

import scala.collection.immutable.HashMap
import scala.collection.immutable.WrappedString
import scala.util.Random 
import scala.actors.Actor
import scala.actors.Actor._
import org.anddev.andengine.util.Debug

abstract class Money 
case class UncheckedM(m:Int) extends Money
case class CheckedM(m:Int) extends Money

abstract class Caze
case class CheckedC(caze:Case) extends Caze
case class UncheckedC(caze:(Int,Int)) extends Caze

abstract class LCazes
case class CheckedL(cazes:PorMov) extends LCazes
case class UncheckedL(cazes:List[(Int,Int)]) extends LCazes

abstract class Action
case class Give(howmuch:Money,to:Int) extends Action
case class Deplacement(dep:LCazes) extends Action
case class Joindre(dep:LCazes) extends Action
case class Attaque(from:Caze,to:Caze) extends Action
case class Capture(caze:Caze) extends Action
case class Produire(caze:Caze,unite:Unite) extends Action
case class Detruire(caze:Caze) extends Action
case class Embarquement(dep:LCazes) extends Action
abstract class Event extends Action
case class Equipe(z:Statut,j:Int) extends Event
case class Message(s:String) extends Event
case class MessageT(s:String) extends Event
case class MessageJ(s:String,j:Joueur) extends Event
case class Answer(b:Boolean) extends Event
case class Start() extends Event
case class StartT() extends Event
case class Next() extends Event
case class GiveUp() extends Event

case class Param(val mByB:Int)

class Partie(crecarte:Carte,joueurstart:List[Joueur],param:Param) extends Actor
{

  var lactions:List[Action] = List()

  var tour = 0
  var historique:List[(Action,Carte,List[Int])] = List((Start(),crecarte,List()))
  val tabjrs:IndexedSeq[List[Joueur]] = IndexedSeq(joueurstart.map(x=>List(x)):_*)
  override def toString()=historique.head.toString()
  def tourn = tour/joueurstart.length
  def tourj = tour % joueurstart.length
  def joueura = joueur(tourj)
  def carte = historique.head._2
  def joueur(id:Int):Joueur = tabjrs(id%joueurstart.length).head
  def add(a:Action,carte:Carte=carte,li:List[Joueur]=List()) = historique::=(a,carte,update(li))
  def update(jrs:List[Joueur])= {
    jrs.foreach(j=>tabjrs.updated(j.id,j::tabjrs(j.id)))
    jrs.map(_.id)
  }
  def joueurs = tabjrs.map(_.head).toList
  def next = {
    if (!(joueura.aperdu)) {
    val resunit = HashMap(joueura.unitz(carte).mapValues((x) => x.reactiverUnite).toSeq:_*)
    val rescarte = carte.factory(carte.unitz ++: resunit)
    add(Next(),rescarte)
    tour +=1
    }
    startp(tourj)
  }
  def back = {
    historique.head._3.foreach(x => tabjrs.updated(x,tabjrs(x).tail))
    historique = historique.tail                               
    
  }
  def reset = {
    historique = List(historique.last)
    tour = 0
    startp(tour)
  }
  startp(tour)

  def act() {
    while(true) {
      receive { 
        case (j:Joueur,x:Action) => checkA(x,j).foreach(doIt(_,j))
        case _ => ()
             }
    }
  }
  def doIt(a:Action,j:Joueur) = {
      val done:(Option[Carte],Option[List[Joueur]]) = a match {
        case Attaque(CheckedC(from),CheckedC(to))  if (j.id==tourj) => (Some(carte.attaque(from,to).get),None)
        case Give(CheckedM(cb),to)  if (j.id==tourj) => (None,Some(j.give(cb,joueur(to))))
        case Deplacement(CheckedL(pormov))  if (j.id==tourj) => (Some(carte.deplace(pormov)),None)
        case Capture(CheckedC(caze))  if (j.id==tourj) => (Some(carte.capture(caze)),None)
        case Joindre(CheckedL(dep))  if (j.id==tourj) =>  (Some(carte.joindre(dep.caz,dep.cazf)),None)
        case Embarquement(CheckedL(dep))  if (j.id==tourj) => (Some(carte.embarquement(dep.caz,dep.cazf)),None)
        case Produire(CheckedC(caze),unite)  if (j.id==tourj) => {
          val pro = carte produire(caze,unite,j)
          (Some(pro._1),Some(List(pro._2)))
        }
        case Detruire(CheckedC(caze))  if (j.id==tourj) => (Some(carte.update(None,caze)),None)
        case Next()  if (j.id==tourj) => { next; (None,None) }
        case GiveUp()  if (j.id==tourj) => (None,None)
        case Equipe(st,z) if (st == Guerre()) => (Some(carte.nvStatut(j.id,z,st)),None)
        case Message(msg) => { println(msg); (None,None) }
        case MessageT(msg) => { println(msg); (None,None) }
        case _ => (None,None)
      }
      if (done._1.isDefined) {
        if (done._2.isDefined) add(a,done._1.get,done._2.get)
        else add(a,done._1.get)
      }
      else {
        if (done._2.isDefined) {
          add(a,carte,done._2.get)
        }
      }
    }
  

  def checkL(lis:List[(Int,Int)]) = { 
    if (lis.forall(carte.getCase(_).isDefined))
      Some(lis.map(carte.getCase(_).get))
    else None
  }
           
def checkA(a:Action,j:Joueur):Option[Action] = {
    if(isTour(j)){
      a match {
        case Attaque(UncheckedC(from),UncheckedC(to)) =>
          isAttaquable(from,to).map(x =>Attaque(CheckedC(x._1),CheckedC(x._2)))

        case Deplacement(UncheckedL(dep)) =>
          checkL(dep).flatMap(x => carte.isChV(x,true,j.id))
            .map(y => Deplacement(CheckedL(y)))

        case Give(UncheckedM(cb),to) => if (cb > 0) Some(Give(CheckedM(cb),to)) else None 
                                                                 
        case Capture(UncheckedC(caze)) => 
          carte.getCase(caze).filter(x => appJ(x) && carte.isC(x))
            .map(y => Capture(CheckedC(y)))

        case Joindre(UncheckedL(dep)) => 
          checkL(dep).flatMap(x => carte.isChV(x,true,j.id))
            .filter(y => carte.isJoignable(y.caz,y.cazf))
            .map(z => Joindre(CheckedL(z)))

        case Embarquement(UncheckedL(dep)) =>
          checkL(dep).flatMap(x => carte.isChV(x,true,j.id))
            .filter( y => carte.isEmbarquable(y.caz,y.cazf))
            .map(z => Embarquement(CheckedL(z)))        

        case Produire(UncheckedC(caze),unite) => 
          carte.getCase(caze).filter(x => carte.isProductible(x,unite,j))
            .map(x => Produire(CheckedC(x),unite)) 

        case Detruire(UncheckedC(caze)) =>
          carte.getCase(caze).filter(x => appJ(x)).map(x => Detruire(CheckedC(x)))

        case _ => Some(a)

      }
    } else None
  }


  def isAttaquable(fromc:(Int,Int),toc:(Int,Int)):Option[(Case,Case)] = checkco(fromc,toc).filter(x=> (carte.canAttaque(x._1,x._2)))
  def appJ(caze:Case) = carte.getUnite(caze).get.joueur == joueura.id


  def checkco(fromc:(Int,Int),toc:(Int,Int)) = {
    (carte.getCase(fromc),carte.getCase(toc)) match {
        case (Some(from),Some(to)) => Some(from,to).filter(x=>appJ(x._1))
        case _ => None
  }
}




    def isTour(joueur:Joueur) = joueura.id == joueur.id
    def isTour(joueur:Int) = joueura.id == joueur

    def inMove(caz:Case) = carte.inMove(caz)

    def startp(tourj:Int) = {
      val player = joueur(tour)
      player.startp(this)
      def repareB(hmunitz:HashMap[Case,Unite],x:Int):(Int,HashMap[Case,Unite]) = {
        player.batiz(hmunitz).foldLeft((0,HashMap[Case,Unite]()))((acc,cazunit) => {
          val repareU = cazunit._2.reparer(x)
          (acc._1 + repareU._1,acc._2+((cazunit._1,repareU._2)))})    
      }

      def getMoney = player.money(player.batiz(carte).foldLeft(0)( (acc,b) => acc + param.mByB))
      val reparation = repareB(player.unitz(carte),20) 
      add(StartT(),carte.factory(carte.unitz ++: reparation._2),List(getMoney.money(-(reparation._1))))
    }

  }    
  
  
abstract class Joueur(name:String,val bourse:Int,val id:Int,val aperdu:Boolean=false){
    override def toString()= "J > Nom:" +name +" & Bourse: " +bourse.toString
    def money(x:Int):Joueur// = new Joueur(name,bourse +x,equipe,id)
    def give(x:Int,y:Joueur) = List(money(-x),y.money(x))
    def unitz(carte:Carte):HashMap[Case,Unite] = carte.getUnitzJ.get(id).getOrElse(HashMap[Case,Unite]())
    def batiz(hmunitz:HashMap[Case,Unite]):HashMap[Case,Unite] = hmunitz.filter( kv => !(kv._1.dessus.isEmpty))
    def batiz(carte:Carte):HashMap[Case,Unite]  = batiz(carte.getUnitzJ.get(id).getOrElse(HashMap[Case,Unite]()))
    def isTour(partie:Partie) = partie.isTour(this)
    def perdre:Joueur// = new Joueur(name,bourse,equipe,id,true)
    def startp(x:Partie):Unit = ()
  }

case  class Local(name:String,override val bourse:Int, override val id:Int,override val  aperdu:Boolean=false) extends Joueur(name,bourse,id,aperdu) {
  def money(x:Int)=copy(bourse=bourse+x)
  def perdre=copy(aperdu=true)
  override def startp(partie:Partie) = {}
}

case class IA(ia:Int,name:String,override val bourse:Int,override val id:Int,override val  aperdu:Boolean=false) extends Joueur(name,bourse,id,aperdu) {
  def money(x:Int)=copy(bourse=bourse+x)
  def perdre=copy(aperdu=true)
}

case class Remote(val name:String,override val bourse:Int,override val id:Int,override val aperdu:Boolean,remote:ServerClient) extends Joueur(name,bourse,id,aperdu) {
  def money(x:Int)=copy(bourse=bourse+x)
  def perdre=copy(aperdu=true)
}



object Joueur {
    var id = -1
    val r = new Random()
    def apply() = {id+=1; Local(r.nextInt(16546).toString(),0,id)}
    def apply(str:String)= {id+=1;Local(str,0,id)}
  }


