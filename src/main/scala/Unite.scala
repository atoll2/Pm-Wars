package adwap.android.project
import Math.min


trait Artillerie {
  val range:(Int,Int)
}

trait Stealth {
  val stealth=false
}
trait ArmeSecondaire {

  val puissanceS:Int
  val perforationS:Int

}

trait Embarcadaire {
  val lEmbarc:List[Unite]
  val contient:List[Unite]
  val chargement:Int
  def embarquer(x:Unite):Unite
}
trait Captur{
val pdv:Int
val joueur:Int
}


abstract class Unite() extends Mode{
  val id:Int
  val pdv:Int
  val munition:Int
  val acombattu:Boolean
  val mov_e:Int
  val joueur:Int
  val typ:String

  val perforation:Int
  val mov:Int
  val munitionMax:Int
  val puissance:Int
  val cout:Int
  val blindage: Int
  val vision:Int
  val coefpdv:Int = 1

  def factory(podv:Int,mounition:Int=munition,acomb:Boolean=true,movr:Int=mov_e,joueur:Int=joueur):Unite
  def degat(puis:Int,perfo:Int,blind:Int):Int = puis*pdv/10*((perfo-blind).max(0))
  def degat(blind:Int):Int = degat(puissance,perforation,blind)
  def reactiverUnite = factory(pdv,munition,false,0)
  def desactiverUnite = factory(pdv,munition,true,0)
  def reparer(x:Int) = { 
    val repar = (x + pdv) min 100
    (cout/pdv*repar,factory(repar,munitionMax,acombattu,mov_e,joueur))
  }
  def joindre(x:Unite)=factory((pdv+x.pdv).min(100),(munition-x.munition-munitionMax).max(0))

  override def toString() = "pdv: " +pdv +" type:" +typ

}


abstract class Vehicule() extends Unite(){
         
           val fuelMax:Int
           override def reparer(x:Int) = (cout/100*x,factory(pdv+x,munitionMax,fuelMax,acombattu,mov_e,joueur))
           def factory(pdv:Int,munition:Int,fuel:Int,acombattu:Boolean,mov_e:Int,joueur:Int):Vehicule
                                                 

}

abstract class Infanterie() extends Unite() with Marchable with Captur

case class Bazooka(val pdv:Int,val munition:Int,
                   val acombattu:Boolean,val mov_e:Int, val joueur:Int) 
     extends Infanterie() with ArmeSecondaire
{

  def this(j:Int)=this(100,0,false,0,j)
  def factory(podv:Int,mounition:Int,acomb:Boolean,movr:Int,j:Int)=  copy(pdv=podv,munition=mounition,acombattu=acomb,mov_e=movr,joueur=j)
  val typ = "Bazooka"
  val cout = 3000
  val puissanceS = 8
  val perforationS = 1
  val vision = 8
  val blindage = 2
  val puissance = 30
  val perforation = 1
  val mov = 10
  val munitionMax = 4
  val id = 2


}

case class Fantassin(val pdv:Int,val acombattu:Boolean,
                     val mov_e:Int,val joueur:Int)
     extends Infanterie() with ArmeSecondaire
{
  
  def this(joueur:Int)=this(100,false,0,joueur)
  def factory(podv:Int,whatever:Int,acomb:Boolean,movr:Int,j:Int)= copy(pdv=podv,acombattu=acomb,mov_e=movr,joueur=j)
  val typ = "Fantassin"
  val cout = 1000
  val puissanceS = 2
  val perforationS = 4
  val vision = 10
  val blindage = 1
  val puissance = 0
  val perforation = 0
  val mov = 20
  val munitionMax = 0
  val munition = 0
  val id = 1
}
