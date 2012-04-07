package adwap.android.project
import scala.collection.immutable.HashMap
import scala.collection.immutable.WrappedString
import scala.util.Random 


abstract class Caze
case class CheckedC(caze:Case) extends Caze
case class UncheckedC(caze:(Int,Int)) extends Caze

abstract class LCazes
case class CheckedL(cazes:PorMov) extends LCazes
case class UncheckedL(cazes:List[(Int,Int)]) extends LCazes

abstract class Action
case class Deplacement(dep:LCazes) extends Action
case class Joindre(dep:LCazes) extends Action
case class Attaque(from:Caze,to:Caze) extends Action
case class Capture(caze:Caze) extends Action
case class Produire(caze:Caze,unite:Unite) extends Action
case class Detruire(caze:Caze) extends Action
case class Embarquement(dep:LCazes) extends Action
abstract class Event extends Action
case class Start() extends Event
case class StartT() extends Event
case class Next() extends Event
case class GiveUp() extends Event

case class Param(val mByB:Int)

class Partie(crecarte:Carte,joueurstart:List[Joueur],param:Param)
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
  def joueur(id:Int) = tabjrs(id).head
  def add(a:Action,carte:Carte=carte,li:List[Joueur]=List()) = (a,carte,update(li))::historique
  def update(jrs:List[Joueur])= {
    jrs.foreach(j=>tabjrs.updated(j.id,j::tabjrs(j.id)))
    jrs.map(_.id)
  }
  def next = {
    val resunit = HashMap(joueura.unitz(carte).mapValues((x) => x.reactiverUnite).toSeq:_*)
    val rescarte = carte.factory(carte.unitz ++: resunit)
    add(Next(),rescarte)
    tour +=1
    start(tourj)
  }
  def reset = {
    historique = List(historique.last)
    tour = 0
    next
  }
  start(tour)

  def doIt(a:Action,joueur:Joueur) = {
    if(joueur.id==tourj) {
      val done:(Option[Carte],Option[List[Joueur]]) = a match {
        case Attaque(CheckedC(from),CheckedC(to)) => (Some(carte.attaque(from,to).get),None)
        case Deplacement(CheckedL(pormov)) => (Some(carte.deplace(pormov)),None)
        case Capture(CheckedC(caze)) => (Some(carte.capture(caze)),None)
        case Joindre(CheckedL(dep)) =>  (Some(carte.joindre(dep.caz,dep.cazf)),None)
        case Embarquement(CheckedL(dep)) => (Some(carte.embarquement(dep.caz,dep.cazf)),None)
        case Produire(CheckedC(caze),unite) => {
          val pro = carte produire(caze,unite,joueur)
          (Some(pro._1),Some(List(pro._2)))
        }
        case Detruire(CheckedC(caze)) => (Some(carte.update(None,caze)),None)
        case Next() => (None,None)
        case GiveUp() => (None,None)
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
  }

  def checkL(lis:List[(Int,Int)]) = { 
    if (lis.forall(carte.getCase(_).isDefined))
      Some(lis.map(carte.getCase(_).get))
    else None
  }
           
def checkA(a:Action,joueur:Joueur):Option[Action] = {
    if(isTour(joueur)){
      a match {
        case Attaque(UncheckedC(from),UncheckedC(to)) =>
          isAttaquable(from,to).map(x =>Attaque(CheckedC(x._1),CheckedC(x._2)))

        case Deplacement(UncheckedL(dep)) =>
          checkL(dep).flatMap(x => carte.isChV(x,true,joueur.id))
            .map(y => Deplacement(CheckedL(y)))
                                                                 
        case Capture(UncheckedC(caze)) => 
          carte.getCase(caze).filter(x => appJ(x) && carte.isC(x))
            .map(y => Capture(CheckedC(y)))

        case Joindre(UncheckedL(dep)) => 
          checkL(dep).flatMap(x => carte.isChV(x,true,joueur.id))
            .filter(y => carte.isJoignable(y.caz,y.cazf))
            .map(z => Joindre(CheckedL(z)))

        case Embarquement(UncheckedL(dep)) =>
          checkL(dep).flatMap(x => carte.isChV(x,true,joueur.id))
            .filter( y => carte.isEmbarquable(y.caz,y.cazf))
            .map(z => Embarquement(CheckedL(z)))        

        case Produire(UncheckedC(caze),unite) => 
          carte.getCase(caze).filter(x => carte.isProductible(x,unite,joueur))
            .map(x => Produire(CheckedC(x),unite)) 

        case Detruire(UncheckedC(caze)) =>
          carte.getCase(caze).filter(x => appJ(x)).map(x => Detruire(CheckedC(x)))

        case _ => None

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


    def inMove(caz:Case) = carte.inMove(caz)

    def start(tourj:Int) = {
      val player = joueur(tour)
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
  
  
  class Joueur(name:String,val bourse:Int,equipe:Equipe,val id:Int){
    override def toString()= "J > Nom:" +name +" & Bourse: " +bourse.toString
    def money(x:Int) = new Joueur(name,bourse +x,equipe,id)
    def unitz(carte:Carte):HashMap[Case,Unite] = carte.getUnitzJ.get(id).getOrElse(HashMap[Case,Unite]())
    def batiz(hmunitz:HashMap[Case,Unite]):HashMap[Case,Unite] = hmunitz.filter( kv => !(kv._1.dessus.isEmpty))
    def batiz(carte:Carte):HashMap[Case,Unite]  = batiz(carte.getUnitzJ.get(id).getOrElse(HashMap[Case,Unite]()))
    def isTour(partie:Partie) = partie.isTour(this)
  }                                            

  case class Equipe(numero:Int)

  object Joueur {
    var id = 0
    val r = new Random()
    def apply(equipe:Equipe):Joueur = { id+=1; new Joueur(r.nextInt(16546).toString(),0,equipe,id)}
    def apply(equipe:Equipe,str:String):Joueur = {id+=1;new Joueur(str,0,equipe,id)}
  }






