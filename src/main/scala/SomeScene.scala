package adwap.android.project

import org.anddev.andengine.entity.scene.Scene
import org.anddev.andengine.engine.camera.Camera
import org.anddev.andengine.util.Debug
import org.anddev.andengine.input.touch.TouchEvent
import org.anddev.andengine.input.touch.detector.ClickDetector
import org.anddev.andengine.input.touch.detector.ClickDetector.IClickDetectorListener
import org.anddev.andengine.input.touch.detector.ScrollDetector
import org.anddev.andengine.input.touch.detector.ScrollDetector.IScrollDetectorListener
import org.anddev.andengine.input.touch.detector.SurfaceScrollDetector
import org.anddev.andengine.entity.scene.Scene.IOnSceneTouchListener
import org.anddev.andengine.entity.text._
import org.anddev.andengine.entity.primitive.Rectangle
import org.anddev.andengine.engine.camera.hud._
import org.anddev.andengine.opengl.font.Font



class SuperHUD(pCamera:Camera,font:Font,fontO:Font,ma:MainActivity) extends HUD with IOnSceneTouchListener with IScrollDetectorListener with IClickDetectorListener {
  val mClickDetector = new ClickDetector(this)
  mClickDetector.setEnabled(true)
  
  val mScrollDetector = new ScrollDetector(this)
  mScrollDetector.setEnabled(false)

  super.setCamera(pCamera)

  val njoueur  = new ChangeableText(0,0,font,"No One",10)
  def setJ(joueur:Joueur) = njoueur.setText(joueur.name)
  attachChild(njoueur)




  override def onClick(pClickDetector:ClickDetector,pTouchEvent:TouchEvent)
  {
    val (x,y)= (pTouchEvent.getX(),pTouchEvent.getY())
  }

  val (xc,yc)= (pCamera.getWidth,pCamera.getHeight)
  val taille_mhud = yc/6
  val rec =new Rectangle(0,yc-taille_mhud,xc,taille_mhud)
  rec.setColor(0.023f,0.153f,0.213f,0.9f)
  rec.setVisible(false)
  
  attachChild(rec)

  val next = new Text(xc*5/7,0,font,"NEXT"){
    override def onAreaTouched( pSceneTouchEvent:TouchEvent, pTouchAreaLocalX:Float, pTouchAreaLocalY:Float) = {
        if(pSceneTouchEvent.getAction() == TouchEvent.ACTION_UP){
                ma.next
        }
                true
              }
  }
  registerTouchArea(next)

  attachChild(next)


  var ls:List[Text]=List()
  def addO(ord:List[Action],j:Int){
    rec.setVisible(true)
    val cancel = new Text(xc/3-50,taille_mhud/3,fontO,"Cancel"){
	      override def onAreaTouched( pSceneTouchEvent:TouchEvent, pTouchAreaLocalX:Float, pTouchAreaLocalY:Float) = {
                if(pSceneTouchEvent.getAction() == TouchEvent.ACTION_UP){                  
                  ls.foreach(z => ma.effacersprite(z))
                  ls = List()
                  rec.setVisible(false)
                }
                true
              }

      }
    ls ::= cancel
    rec.attachChild(cancel)
    registerTouchArea(cancel)
    var i=1
    ord.foreach(y => {
      val txt = y match {
        case Capture(_) => "Capturer"
        case Joindre(_) => "Joindre"
      }
      val ordre = new Text(xc/3-50+i*150,taille_mhud/3,fontO,txt){
	      override def onAreaTouched( pSceneTouchEvent:TouchEvent, pTouchAreaLocalX:Float, pTouchAreaLocalY:Float) = {
                if(pSceneTouchEvent.getAction() == TouchEvent.ACTION_UP){                  
                  ma.eval(List(y),j,false)                
                  del
                }
                true
              }

      }
      ls ::= ordre
      rec.attachChild(ordre)
      registerTouchArea(ordre)
      i+=1
    })
  }
  

  
  def del = {
    ls.foreach(z => { ma.effacersprite(z);unregisterTouchArea(z)})
    ls = List()
    rec.setVisible(false)
  }

 override def onScroll(pScrollDetector:ScrollDetector,pTouchEvent:TouchEvent,pDistanceX:Float,pDistanceY:Float) = {
  }
  

 override def  onSceneTouchEvent(pScene:Scene, pSceneTouchEvent:TouchEvent)= {
   if(pSceneTouchEvent.getY() >= yc-taille_mhud && rec.isVisible()) {
     mClickDetector.onTouchEvent(pSceneTouchEvent)
     true
   }
   else false
  }

  setOnSceneTouchListener(this)
  setTouchAreaBindingEnabled(true)


}
