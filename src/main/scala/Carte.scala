package adwap.android.project

import scala.math._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.HashSet
import scala.collection.immutable.HashMap
import scala.collection.immutable.StringOps
import java.io.InputStream
import org.anddev.andengine.util.Debug

abstract class TypeMap
case class Iles() extends TypeMap
case class Continent() extends TypeMap

abstract trait Mode
abstract trait Environnement[+T <: Mode] {
  def cost(T:Mode):Option[Int]
  val defe_inf = 0
  val defe_veh = 0
  val furtivite = 0
  val vision = 0
  val stealth = 0

}
abstract trait NaviEnvi extends Environnement[Naviguable]
{ def cost(x:Naviguable) = Some(1) 
  def cost(x:Mode) = None
}
abstract trait SurvoEnvi extends Environnement[Survolable]
{ def cost(x:Survolable) = Some(1)
  def cost(x:Mode) = None
 }
trait Naviguable  extends Mode
trait Survolable  extends Mode 
trait Marchable   extends Mode
trait Chenillable extends Marchable  
trait Roulable extends Chenillable
trait Capturable extends Mode {
  val joueur:Option[Int]
  val captur:Int
  def factory(de:Int,j:Option[Int]):Batiment 
  def capture(de:Int,j:Int):Batiment= { 
    val n = (captur - de)
      if (n > 0) factory(n,joueur)
      else factory(20,Some(j))
  }
}

case class Plaine() extends Environnement[Roulable] {
  def cost(x:Mode) = x match {
    case x:Roulable => Some(10)
    case x:Chenillable => Some(5)
    case x:Marchable => Some(4)
    case _ => None
  }
}

case class Foret() extends  Environnement[Roulable] {
  override val defe_inf = 1
  override val defe_veh = 2
  override val furtivite = 80

  def cost(x:Mode) = x match {
    case x:Roulable => Some(10)
    case x:Chenillable => Some(5)
    case x:Marchable => Some(9)
    case _ => None
  }

}

case class Montagne() extends Environnement[Marchable] { 

  def cost(x:Mode) = x match {
    case x:Marchable => Some(5)
    case _ => None
  }
  override val vision = 50
}


case class Mer()  extends NaviEnvi {
}



abstract class Batiment(val joueur:Option[Int]) {
  override def toString() = (getClass().getSimpleName() +" " +joueur.getOrElse("").toString())
  val defe_veh = 0
  val defe_inf = 0
}






abstract class Usine(override val joueur:Option[Int]) extends Batiment(joueur) {
  val protection = 0 
  val production:Option[List[Unite]]
}
case class Barraque(override val joueur:Option[Int]) extends Usine(joueur) {
  val production = joueur.map(x => List(new Fantassin(x)))
}
case class Garage(override val joueur:Option[Int]) extends Usine(joueur){
  val production = None
}
case class Port(override val joueur:Option[Int]) extends Usine(joueur){
  val production = None
}
case class Airport(override val joueur:Option[Int]) extends Usine(joueur){
  val production = None
}

abstract  class Fortification(override val joueur:Option[Int]) extends Batiment(joueur)
case class HQ(override val joueur:Option[Int]) extends Fortification(joueur)
case class Ville(override val joueur:Option[Int],val captur:Int) extends Fortification(joueur) with Capturable {
  def factory(de:Int,j:Option[Int])=Ville(j,de)
}

case class Case(val typ:Environnement[Mode],val dessus:Option[Batiment], val coord:(Int,Int)){

  def factory(dess:Option[Batiment]) = Case(typ,dess,coord)
  def dess(c:Boolean)= if (!c) "_|" else "X|"
  def defe(typu:Unite)= typu match {
    case x:Infanterie => typ.defe_inf +  { if (dessus.isEmpty) 0 else dessus.get.defe_inf }
    case x:Vehicule => typ.defe_veh + { if (dessus.isEmpty) 0 else dessus.get.defe_veh }
  }
  override def toString() = "Case : "+coord.toString +" " +dessus.getOrElse("").toString()
}

case class PorMov(path:List[Case],cost:Int) {
  val (caz,cazf)=(path.head,path.last)
  override def toString()= caz.toString()+"-->"+cazf.toString()
  def moinsFinal(x:Mode)=(PorMov(path.tail,cost-cazf.typ.cost(x).get),cazf)


}

abstract class Statut
case class Guerre() extends Statut
case class Paix() extends Statut
case class Alliance() extends Statut

abstract class Carte() {
  
  override def toString() = { 
    carteflat.filter(x => x.dessus.isDefined).foreach(println(_))
    getUnitzJ.toString()
  }
  def apply(x:(Int,Int)):Case

  def dess(list:List[Case])= {
    val l =  list map ( (x:Case) => x.coord) 
    val (x,y) = (largeur-1,hauteur-1)
      for (i <-0 to y; j <- 0 to x) {
        print(getCase(j,i).get.dess(l contains (j,i)))
        if ((j+1)%(x+1) == 0) println("")
      }
  }
  
  
  val equipe:IndexedSeq[IndexedSeq[Statut]]
  val carte:IndexedSeq[IndexedSeq[Case]]
  val carteflat=carte.flatten
  def getUnitz:List[(Case,Unite)]
  def getUnitzJ:Map[Int,HashMap[Case,Unite]]
  val unitz:HashMap[Case,Unite]
  
  def nvStatut(x:Int,y:Int,z:Statut):Carte
  
  def isProductible(caze:Case,p:Unite,joueur:Joueur) = (joueur.bourse > p.cout) && (caze.dessus.getOrElse(false) match {
    case x:Usine => x.production.exists( _ contains p)
    case _ => false
  })

  def produire(caze:Case,p:Unite,j:Joueur):(Carte,Joueur)=(update(Some(p),caze),j.money(-(p.cout)))

  def isChV(dep:List[Case],b:Boolean,id:Int) = {
    val uniteO = getUnite(dep.head)
    if (uniteO.isDefined) {
    def chv(lpa:List[Case]):(Boolean,Int)={
      lpa match {
        case x ::(y :: z) =>  {
          val costO= y.typ.cost(uniteO.get)
          if (costO.isDefined) {
            val bo = (acote(x).contains(y)) && getUnite(y).forall(x => equipe(id)(x.joueur)!=Guerre) && ( z!=Nil || b) 
            val r = chv(y::z)
            (bo&&r._1,r._2+(y.typ.cost(uniteO.get).get))
          }
          else (false,0)
        }
        case x :: Nil => (true,0)
        case _ => (false,0)
      }
    }
      val s = chv(dep)
      if (s._1) Some(PorMov(dep,s._2))
      else None
    }
      else None
  }

  def isC(co:Case) = {
    (getUnite(co),co.dessus) match {
      case (Some(x:Captur),Some(y)) => y.joueur.forall(z => x.joueur != z)
      case _ => false
    }
  }

  def isJoignable(from:Case,to:Case) = {
    (getUnite(from),getUnite(to)) match {
      case (Some(x),Some(y)) => (x.joueur == y.joueur) && (x.getClass == y.getClass)
      case _ => false
    }
  }
  def isEmbarquable(from:Case,to:Case) = {
    (getUnite(from),getUnite(to)) match {
      case (Some(x),Some(y:Embarcadaire)) => (x.joueur == y.joueur) && (y.lEmbarc contains x.getClass) &&  (y.contient.length<y.chargement)
      case _ => false
    }
  }
  
  def joindre(from:Case,to:Case)= {
    factory(update(List((to,Some(getUnite(to).get.joindre(getUnite(from).get))),(from,None))))
  }
  def embarquement(from:Case,to:Case) = {
    getUnite(to) match {
      case Some(x:Embarcadaire) => factory(update(List((to,Some(x.embarquer(getUnite(from).get))),(from,None))))
      case _ => this
    }
  }

  def capture(caze:Case) ={ 

    (caze.dessus,getUnite(caze)) match {
    case (Some(x:Capturable),Some(y:Captur)) => {
      val ncase = caze.factory(Some(x.capture(y.pdv/10,y.joueur)))
      val ncarte = update(Some(y.desactiverUnite),caze) 
      ncarte.remplacer(caze,ncase)
      }
    case _ => this
  }
                         }
  def attaqueUni(unite:Unite,ennemi:Unite,caz:Case) = {
      def create(pdvr:Int,m:Boolean):(Option[Unite],Option[Unite]) = {
        val thisshit = Some(unite.factory(unite.pdv,acomb=true,mounition = {if (m) unite.munition+1 else unite.munition}))
        if (pdvr > 1) (thisshit,Some(ennemi.factory(pdvr,mounition=ennemi.munition,joueur = ennemi.joueur))) 
        else (thisshit,None)
      }
    if (unite.munition < unite.munitionMax) create(ennemi.pdv - unite.degat(ennemi.blindage),true)
    else unite  match {

      case x:ArmeSecondaire => {create(ennemi.pdv - unite.degat(x.puissanceS,x.perforationS,(ennemi.blindage + caz.defe(ennemi))),false)}
      case _ => (Some(unite),Some(ennemi))
    }
  }


  def canAttaque(cazunit:Case,cazennemi:Case) = getUnite(cazunit).exists(x=> getUnite(cazennemi).isDefined && (!(x.acombattu)) && {
            x match {
              case x:Artillerie => isInRange(cazunit,cazennemi)
              case _ => (acote(cazunit).contains(cazennemi))
            }
          })



      
  
  def attaqueP(cazunit:Case,cazennemi:Case):(Option[Unite],Option[Unite]) = {
    val (unite,ennemi) = (getUnite(cazunit).get,getUnite(cazennemi).get)
          (unite,ennemi) match {
            case (x:Artillerie,y) => attaqueUni(x,y,cazennemi)
            case (x,y:Artillerie) => attaqueUni(x,y,cazennemi)
            case (x,y) => { 
                val first = attaqueUni(x,y,cazennemi)
                first match {
                  case (Some(x:Unite),Some(y:Unite)) => {
                    val second = attaqueUni(y,x,cazunit)
                    (second._2,second._1)
                  }
                  case (Some(x:Unite),None) => first
                  case _ => first
                }              
            }                  
          }
}


  def attaque(cazunit:Case,cazennemi:Case):Option[Carte]= {
    if (canAttaque(cazunit,cazennemi)) {
      val (x,y) = attaqueP(cazunit,cazennemi)
      Some(factory(update(List((cazunit,x),(cazennemi,y.map(_.reactiverUnite))))))
    }
    else None
  }

  
  def remplacer(x:Case,y:Case):Carte
  def update(cazunitl:List[(Case,Option[Unite])]):HashMap[Case,Unite] 
  def update(unite:Option[Unite],caze:Case):Carte
  def update(caz:Case):Carte
  def deplace(cazdep:PorMov):Carte

  // def pathf(unite:Unite,caz:Case)=pathf(getCase(unite).get,caz)
  def pathf(cazunit:Case,cazennemi:Case,j:Int=(-1)):PorMov= {
    val pa = PathFinder.AStar(this,cazunit,cazennemi,getUnite(cazunit).get,PathFinder.dis_man)
    PorMov(pa._2.reverse,pa._1)
  }
  
  def inMove(caz:Case):List[PorMov]={
    val unite = getUnite(caz).get
    PathFinder.djikstra(this,caz,unite,(unite.mov - unite.mov_e))    
  }


  def factory(unit:HashMap[Case, Unite]):Carte
  def factory(cart:IndexedSeq[IndexedSeq[Case]]):Carte
  def factory(cart:IndexedSeq[IndexedSeq[Case]],unit:HashMap[Case, Unite]):Carte

  def getUnite(caz:Case):Option[Unite] = getUnite(caz.coord)
  def getUnite(coord:(Int,Int)):Option[Unite]

  def getCase(unit:Unite):Option[Case]
  def getCase(coord:(Int,Int)):Option[Case]


  def range(case1:Case,case2:Case):Int

  val contact:List[(Int,Int)]

  def acote(caz:Case):List[Case]= {
    contact.foldLeft[List[Case]](List())((acc,pos) => { 
      val cas = getCase((caz.coord._1+pos._1,caz.coord._2+pos._2))
      cas match { 
        case Some(x:Case) => x::acc
        case None => acc }
    })
  }
  
  // def inRange(unite:Unite,min:Int,max:Int) = inRange(getCase(unite).get,min,max)
  def inRange(cazi:Case,min:Int,max:Int):List[Case] = {
    def recInRange(caz:Case,pas:Int):List[Case] =  {
      if (pas < 1) List(caz) 
      else { val list =  acote(caz) flatMap { case x:Case => recInRange(x, pas-1)                                           }
            
            (caz :: list) distinct
          }
    }
    recInRange(cazi,max) filterNot (recInRange(cazi,min) contains) 
  }

  def isInRange(caz:Case,caze:Case) = getUnite(caz).exists(y => y match {
    case x:Artillerie => inRange(caz,x.range._1,x.range._2) contains caze
    case _ => acote(caz) contains caze
  })
                                                         

 def EveryAction(caz:Case):Option[(Map[Case,List[Action]],List[Case])] = {

   getUnite(caz).map(y => 
     {
       val achack = (PorMov(List(caz,acote(caz).head,caz),0))
       val move_ar = achack::inMove(caz)
       val attaquable = move_ar.foldLeft((Map[Case,List[Action]](),List[Case]()))((acc,pos) => {
         val ac = pos.cazf::acote(pos.cazf)
         val ennemis = ac.filter(x => x != caz && getUnite(x).exists(z=>equipe(z.joueur)(y.joueur)==Guerre()))
         val tom = ennemis.foldLeft(acc._1)((acc2,pos2) => {
           if (isInRange(caz,pos2) && !y.acombattu) {
             acc2 + ((pos2,List(Attaque(CheckedC(caz),CheckedC(pos2)))))
           } else {
             if (acc2 contains pos2) {
               acc2.get(pos2).get.head match {
                 case Deplacement(CheckedL(path)) => {
                   if (pos.cost < path.cost) acc2 + ((pos2,List(Deplacement(CheckedL(pos)))))
                   else acc2
                 }
               }

             } else acc2 + ((pos2,List(Deplacement(CheckedL(pos)))))  }})
         val lat = acc._2 union ac
         (tom,lat)})
       

       
       val mapac = {
         if (y.acombattu) Map[Case,List[Action]]()
         else {move_ar.foldLeft(attaquable._1)((acc,pos) => {
           var la:List[Action] = List()     

           if (getUnite(pos.cazf).isEmpty) {
             la ::= Deplacement(CheckedL(pos))
           }



           getUnite(pos.cazf).foreach(x => {
             if (x.joueur == y.joueur) {
               if (x.getClass() == y.getClass && x!=y) la ::= Joindre(CheckedL(pos))
               else {
                 x match {
                   case x:Embarcadaire => la ::= Embarquement(CheckedL(pos))
                   case _ => ()
                 }
               }         
             }
           })

           if (pos.cazf.dessus.isDefined && pos.cazf.dessus.get.joueur != Some(y.joueur)) {
             la ::= Capture(CheckedC(pos.cazf))
           }
           if (la.isEmpty) acc
           else acc + ((pos.cazf,la.reverse))                                  

         })}}
       (mapac,attaquable._2-caz)
     })}

                    
  def hauteur:Int
  def largeur:Int
}



class CarteTetra(val carte:IndexedSeq[IndexedSeq[Case]],val unitz:HashMap[Case, Unite], val equipe:IndexedSeq[IndexedSeq[Statut]]) extends Carte() {

  def apply(x:(Int,Int))=carte(x._2)(x._1)
  def nvStatut(x:Int,y:Int,z:Statut)= new CarteTetra(carte,unitz,equipe.updated(x,equipe(x).updated(y,z)).updated(y,equipe(y).updated(x,z)))
  def getUnitz = unitz.toList
  val getUnitzJ = unitz.groupBy( kv => kv._2.joueur)
  
  def remplacer(x:Case,y:Case) = {
    val (xx,yy)=x.coord
    val ncarte = carte.updated(yy,carte(yy).updated(xx,y))
    val nunitz= (unitz + ((y,unitz(x))))-x
    Debug.d(nunitz.mapValues(x => x.acombattu).toString)
    new CarteTetra(ncarte,nunitz,equipe)
  }

  def factory(unit:HashMap[Case, Unite]):Carte = factory(carte,unit)
  def factory(cart:IndexedSeq[IndexedSeq[Case]]):Carte= factory(cart,unitz)
  def factory(cart:IndexedSeq[IndexedSeq[Case]],unit:HashMap[Case, Unite]):Carte = new CarteTetra(cart,unit,equipe)

  def delete(caz:Case) = update(unitz,caz,None)
  def update(caz:Case,unite:Option[Unite]):(Option[Unite],HashMap[Case,Unite]) =  update(unitz,caz,unite)
  def update(cazunitl:List[(Case,Option[Unite])]):HashMap[Case,Unite] = {
    cazunitl.foldLeft(unitz)((cart,cazunit) => {
      val up =  update(cart,cazunit._1,cazunit._2) 
      up._2  })}                     
  def update(ounitz:HashMap[Case, Unite],caz:Case, unite:Option[Unite]):(Option[Unite],HashMap[Case,Unite]) = {
    val bef = ounitz.get(caz)
    if (unite.isEmpty) (bef,(ounitz - caz))
    else (bef,ounitz + ((caz,unite.get)))
  }
  def update(unite:Option[Unite],caz:Case):Carte = factory(update(caz,unite)._2)
  def update(caz:Case):Carte=factory(carte.updated(caz.coord._2,carte(caz.coord._2).updated(caz.coord._1,caz)))

  def deplace(cazdep:PorMov):Carte = {
    val unite = getUnite(cazdep.caz).get
    val movr = unite.mov - unite.mov_e


    if (cazdep.cost <= movr && unitz.get(cazdep.cazf).isEmpty) {
      val nunite = unite.factory(unite.pdv,unite.munition,unite.acombattu,(unite.mov_e + cazdep.cost),unite.joueur)
      new CarteTetra(carte,update(delete(cazdep.caz)._2,cazdep.cazf,Some(nunite))._2,equipe)
    } else this
  }



  def getCase(unite:Unite):Option[Case]= unitz.find( (kv) => kv._2 == unite).map(_._1)
  def getCase(coord:(Int,Int)):Option[Case] = {
    val (x,y) = coord
    if (x < 0 || x >= largeur || y >= hauteur || y < 0) None else  Some(carte(y)(x))
  }
  

  def getUnite(coord:(Int,Int)):Option[Unite] = getCase(coord).flatMap(unitz.get(_))
  val contact= List((0,-1),(1,0),(0,1),(-1,0))
  def largeur:Int = if (carte.isEmpty) 0 else carte(0).length 
  def hauteur:Int = carte.length 
  def range(case1:Case,case2:Case) = abs(case1.coord._1 - case2.coord._1) + abs(case1.coord._2 - case2.coord._2)


  
}

object CarteTetra {
  
  val equipeByD = IndexedSeq.fill(20,20)(Guerre())
  def plaine() = {
    def caz(pos:(Int,Int))= Case(Foret(),None,pos)
    def coll = IndexedSeq.tabulate[Case](100,100)((y,x)=> caz((x,y)))
    new CarteTetra(coll,HashMap(),equipeByD)
  }
  
  def creBat(x:Batiment,coord:(Int,Int)) = Case(Plaine(),Some(x),coord)
  def creCase(x:Environnement[Mode],coord:(Int,Int)) = Case(x,None,coord)
  def readChar(c:Char,coord:(Int,Int))= c match {
    case 'P' => creCase(Plaine(),coord)
    case 'M' => creCase(Montagne(),coord)
    case 'F' => creCase(Foret(),coord)
    case 'V' => creBat(Ville(None,20),coord)
  }
  def readCarte(str:String) = {
    val strop = new StringOps(str)
    var (i,j) = (-1,-1)
    val tabl: IndexedSeq[String] = IndexedSeq(strop.split("\n").toSeq:_*)
    val c = tabl.map( x => { i+= 1; j= (-1); x.map( y => {j+=1;readChar(y,(j,i)) })})
    new CarteTetra(c,HashMap(),equipeByD)

  }
  def readFile(str:String):String= {
    val source = scala.io.Source.fromFile(str)
    val lines = source.mkString
    source.close ()
    lines
  }

  def readFile(str:InputStream):String= {
    val source = scala.io.Source.fromInputStream(str)
    val lines = source.mkString
    source.close ()
    lines
  }

}
