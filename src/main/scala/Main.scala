package adwap.android.project

object Test {
  //  val carteb:Carte = CarteTetra.plaine()
  val carteg = CarteTetra.readCarte(CarteTetra.readFile("/home/atoll/androidadwa/src/main/scala/Carte.txt"))

  val (j1,j2,j3) = (Joueur(Equipe(1)),Joueur(Equipe(2)),Joueur(Equipe(3)))  
  val (f1,f2,f3) = (new Fantassin(j1.id), new Fantassin(j2.id),new Fantassin(j3.id))
  val (f,c) = (new Fantassin(j1.id),carteg((2,1)))
  val chem = carteg((1,0))
  val (c1,c2,c3) = (carteg((0,0)),carteg((8,3)),carteg((2,1)))        
  val carte = carteg.factory(carteg.update(List((c1,Some(f1)),(c2,Some(f2)),(c3,Some(f3)),(c,Some(f)))))
    val partie = new Partie(carte,List(j1,j2,j3),Param(1000))
  partie.start
  def main(args: Array[String]) = {




  }
}
