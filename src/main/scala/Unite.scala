package adwap.android.project
import Math.min
import org.anddev.andengine.util.Debug

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


abstract class Unite(val pdv:Int = 100,val munition:Int,val acombattu:Boolean,val mov_e:Int,val joueur:Int) extends Mode{

  val typ:String

  val perforation:Int
  val mov:Int
  val munitionMax:Int
  val puissance:Int
  val cout:Int
  val blindage: Int
  val vision:Int
  val coefpdv:Int = 1

  def factory(podv:Int,mounition:Int=munition,acomb:Boolean=acombattu,movr:Int=mov_e,joueur:Int=joueur):Unite
  def degat(puis:Int,perfo:Int,blind:Int):Int = puis*pdv/10*((perfo-blind).max(0))
  def degat(blind:Int):Int = degat(puissance,perforation,blind)
  def reactiverUnite = factory(pdv,munition,false,0,joueur)
  def reparer(x:Int) = { 
    val repar = x min (x+pdv-100)
    (cout/pdv*repar,factory(pdv+repar,munitionMax,acombattu,mov_e,joueur))
  }
  def joindre(x:Unite)=factory((pdv+x.pdv).min(100),(munition-x.munition-munitionMax).max(0))

  override def toString() = "pdv: " +pdv +" type:" +typ

}


abstract class Vehicule(override val pdv:Int,override val munition:Int, val fuel:Int,
override val acombattu:Boolean, override val mov_e:Int,override val joueur:Int)
         extends Unite(pdv,munition,acombattu,mov_e,joueur){
           val fuelMax:Int
           override def reparer(x:Int) = (cout/100*x,factory(pdv+x,munitionMax,fuelMax,acombattu,mov_e,joueur))
           def factory(pdv:Int,munition:Int,fuel:Int,acombattu:Boolean,mov_e:Int,joueur:Int):Vehicule
                                                 

         }

abstract class Infanterie(override val pdv:Int,override val munition:Int,
                          override val acombattu:Boolean, override val mov_e:Int,override val joueur:Int)
         extends Unite(pdv,munition,acombattu,mov_e,joueur) with Marchable with Captur {
           

         }

class Bazooka(override val pdv:Int,override val munition:Int,override val acombattu:Boolean,
              override val mov_e:Int, override val joueur:Int)
extends Infanterie(pdv,munition,acombattu,mov_e,joueur) with ArmeSecondaire
{

  def this(j:Int)=this(100,0,false,0,j)
  def factory(podv:Int,munition:Int,acomb:Boolean,movr:Int,joueur:Int)=  new Bazooka(podv,munition,acomb,movr,joueur)
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


}

class Fantassin(override val pdv:Int,override val acombattu:Boolean,
                override val mov_e:Int,override val joueur:Int)
extends Infanterie(pdv,0,acombattu,mov_e,joueur) with ArmeSecondaire{
  
  def this(joueur:Int)=this(100,false,0,joueur)
  def factory(podv:Int,whatever:Int,acomb:Boolean,movr:Int,joueur:Int)=  new Fantassin(podv,acomb,movr,joueur)
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
}
