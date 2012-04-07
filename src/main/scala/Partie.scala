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
case class Next extends Event
case class GiveUp extends Event

case class Param(val mByB:Int)

class Partie(crecarte:Carte,joueurstart:List[Joueur],param:Param)
{

  var lactions:List[Action] = List()

  var tour = -1
  var historique:List[(Carte,List[Joueur])] = List((crecarte,joueurstart))
  override def toString()=historique.head.toString()
  next
  def tourn = tour/joueurstart.length
  def tourj = tour % joueurstart.length
  def joueura = joueurs(tourj)
  def next = {
    if (tour >=0) {
      val resunit = HashMap(joueura.unitz(carte).mapValues((x) => x.reactiverUnite).toSeq:_*)
      val rescarte = carte.factory(carte.unitz ++: resunit)
      historique = (rescarte,joueurs)::historique
    }
    tour +=1
    start(joueura)
  }
  def reset = {
    historique = List(historique.last)
    tour = -1
    next
  }

  def doIt(a:Action,joueur:Joueur) = {
      var jrs = joueurs
      val ncarte:Carte= a match {
        case Attaque(CheckedC(from),CheckedC(to)) => carte.attaque(from,to).get
        case Deplacement(CheckedL(pormov)) => carte.deplace(pormov)
        case Capture(CheckedC(caze)) => carte.capture(caze)
        case Joindre(CheckedL(dep)) =>  carte.joindre(dep.caz,dep.cazf)
        case Embarquement(CheckedL(dep)) => carte.embarquement(dep.caz,dep.cazf)
        case Produire(CheckedC(caze),unite) => carte produire(caze,unite)
        case Detruire(CheckedC(caze)) => carte.update(None,caze)
      }
      historique = (ncarte,jrs)::historique    
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
          carte.getCase(caze).filter(x => carte.isProductible(x,unite))
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



    def carte = historique.head._1
    def joueurs = historique.head._2
    def isTour(joueur:Joueur) = joueura.id == joueur.id
//    def deplacer(joueur:Joueur,to:PorMov):(Boolean,String) = {
//      if (isTour(joueur)) {
//        carte.getUnite(to.caz) match {
//          case Some(x:Unite) => if (x.joueur.id == joueur.id) {
//            historique = (carte.deplace(to),joueurs) :: historique
//            (true,"OK")
//          } else (false,"L'unité ne t'appartient pas VILE PERSONNAGE, elle appartient à "+x.joueur.toString())
//            case None => (false,"Ce n'est pas une unité")
//        }
//      } else { (false,"Ce n'est pas ton tour")
//            }
//    }

    def inMove(caz:Case) = carte.inMove(caz)
    def start(player:Joueur) = {

      def repareB(hmunitz:HashMap[Case,Unite],x:Int):(Int,HashMap[Case,Unite]) = {
        player.batiz(hmunitz).foldLeft((0,HashMap[Case,Unite]()))((acc,cazunit) => {
          val repareU = cazunit._2.reparer(x)
          (acc._1 + repareU._1,acc._2+((cazunit._1,repareU._2)))})    
      }

      def getMoney = player.money(player.batiz(carte).foldLeft(0)( (acc,b) => acc + param.mByB))
      val reparation = repareB(player.unitz(carte),20) 
      historique = (carte.factory(carte.unitz ++: reparation._2),joueurs.updated(tourj,getMoney.money(-(reparation._1)))) :: historique
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
    val r = new Random()
    def apply(equipe:Equipe,id:Int):Joueur = new Joueur(r.nextInt(16546).toString(),0,equipe,id)
    def apply(equipe:Equipe,str:String,id:Int):Joueur = new Joueur(str,0,equipe,id)
  }






