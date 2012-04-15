package adwap.android.project

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet

import scala.math._


object PathFinder  {

  def apply(c1:Case,c2:Case,carte:Carte,mode:Mode) = AStar(carte,c1,c2,mode,dis_euc)

  def dis_man(case1:Case,case2:Case):Int = abs(case2.coord._1 - case1.coord._1) + abs(case2.coord._2 - case1.coord._2)*5

  def dis_euc(case1:Case,case2:Case):Int = sqrt(pow((case1.coord._1 - case2.coord._1), 2) + pow((case1.coord._2 - case2.coord._2), 2)).toInt*10

  def djikstra(carte:Carte,start:Case,mode:Mode,costT:Int) = {
	val joueur = carte.getUnite(start).get.joueur
    val startN = NodeD(start,None,0)
    val closedset = HashSet[NodeD]()
    val openset = Queue(startN)
    var current = startN
    
    def reco_path(n:NodeD):List[NodeD] = n.parent match {
      case None => List(n)
      case Some(x) => n::reco_path(x)
    }
    while(!(openset.isEmpty)) {
      current = openset.dequeue()
      if (!(closedset.map(_.caz) contains current.caz)) {
        val adj = current.adjacent(carte,mode,costT).filter(x => carte.getUnite(x.caz).forall(y => carte.equipe(joueur)(y.joueur) != Guerre)).span(closedset  contains _)        
        adj._1.filter(x => x.cost > current.cost + x.caz.typ.cost(mode).get).foreach(x => {
          x.parent = Some(current)
          x.cost = current.cost + x.caz.typ.cost(mode).get})                                                         
        openset.enqueue(adj._2:_*)
        closedset.add(current)
      }
    }
    (closedset-startN).filter(x => carte.getUnite(x.caz).isEmpty).map(x => PorMov(reco_path(x).map(_.caz).reverse,x.cost)).toList
  }

  def AStar(carte:Carte,start:Case,goal:Case,mode:Mode,heur:((Case,Case) => Int),j:Int=(-1)):(Int,List[Case]) = {
    val startN = new NodaA(start,None,0,heur(start,goal))
    val closedset = HashSet[NodaA]()
    val openset = PriorityQueue(startN)(NodaAFOrdering)
    def adj(nod:NodaA) = nod.adjacent(carte,mode,heur,goal,j)
    var current = startN

    def reco_path(y:NodaA):List[Case] = if (y.caz == start) List(start) else  {
      val par:NodaA = y.parent.get
      y.caz::(reco_path(par)) }
    def add(n:NodaA) = { 
      if (!(closedset contains n )) {
      openset.find(_.caz == n.caz) match {
        case Some(no:NodaA) => if (no.g > n.g) { 
          no.parent = Some(current)
          no.g = current.g + no.caz.typ.cost(mode).get
        }
        case None => openset.enqueue(n)
      }
    }
    }
    val lim = 600
    var i = 0
    while(current.caz != goal && i<lim) {
      i+=1        
      current = openset.dequeue()
      if (closedset contains current) () else {
      closedset += current
      
      adj(current).foreach(add(_))}
    }
    if (i==lim) current = closedset.minBy(x=>x.g+x.h*10)
    (current.g, reco_path(current))

  }
}
case class NodeD(val caz:Case,var parent:Option[NodeD],var cost:Int) {
  def adjacent(carte:Carte,mode:Mode,costT:Int) = carte.acote(caz).filter(x => x.typ.cost(mode).exists(y => y + cost <= costT)).map(x => NodeD(x,Some(this),cost + x.typ.cost(mode).get))
}

class NodaA(val caz:Case,var parent:Option[NodaA], var g:Int, val h:Int){
  def adjacent(carte:Carte,mode:Mode,heur:((Case,Case) => Int),goal:Case,j:Int):List[NodaA] = {
    carte.acote(caz).foldLeft[List[NodaA]](List())((acc,pos) => pos.typ.cost(mode) match {
      case Some(x:Int) => if (carte.getUnite(pos).forall(x => carte.equipe(x.joueur)(j) != Guerre)) (new NodaA(pos,Some(this),g+x,heur(pos,goal)))::acc else acc
      case None => acc
    })
  }
  override def toString()= caz.toString +g + " h: " +h
}                                                 


object NodaAFOrdering extends Ordering[NodaA] {
  def compare(x:NodaA,y:NodaA):Int  =  (y.h + y.g)  compare (x.h + x.g)
}


