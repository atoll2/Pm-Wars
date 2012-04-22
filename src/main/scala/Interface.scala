package adwap.android.project

abstract trait Interface{
  
  def capturer(x:Case):Unit
  def deplacer(to:PorMov):Unit
  def next:Unit
  def attaque(x:Case,y:Case)
  def joindre(to:PorMov):Unit
  def produire(x:Case,y:Unite)
  

}
