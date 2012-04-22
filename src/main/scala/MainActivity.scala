package adwap.android.project

import _root_.android.app.Activity
import _root_.android.os.Bundle

import scala.collection.mutable.Map



import javax.microedition.khronos.opengles.GL10;
import org.anddev.andengine.extension.input.touch.controller.MultiTouch
import org.anddev.andengine.extension.input.touch.controller.MultiTouchController
import org.anddev.andengine.extension.input.touch.detector.PinchZoomDetector
import org.anddev.andengine.extension.input.touch.detector.PinchZoomDetector.IPinchZoomDetectorListener
import org.anddev.andengine.extension.input.touch.exception.MultiTouchException
import org.anddev.andengine.input.touch.TouchEvent
import org.anddev.andengine.input.touch.detector.ClickDetector
import org.anddev.andengine.input.touch.detector.ClickDetector.IClickDetectorListener
import org.anddev.andengine.input.touch.detector.ScrollDetector
import org.anddev.andengine.input.touch.detector.ScrollDetector.IScrollDetectorListener
import org.anddev.andengine.input.touch.detector.SurfaceScrollDetector
import org.anddev.andengine.engine.Engine
import org.anddev.andengine.engine.camera.ZoomCamera
import org.anddev.andengine.entity.IEntity
import org.anddev.andengine.engine.options.EngineOptions
import org.anddev.andengine.engine.options.EngineOptions.ScreenOrientation
import org.anddev.andengine.engine.options.resolutionpolicy.RatioResolutionPolicy
import org.anddev.andengine.entity.scene.Scene
import org.anddev.andengine.entity.scene.Scene.IOnSceneTouchListener;
import org.anddev.andengine.entity.scene.background.ColorBackground
import org.anddev.andengine.entity.sprite.Sprite
import org.anddev.andengine.entity.util.FPSLogger
import org.anddev.andengine.entity.text._
import org.anddev.andengine.extension.svg.adt.ISVGColorMapper;
import org.anddev.andengine.extension.svg.adt.SVGDirectColorMapper;
import org.anddev.andengine.extension.svg.opengl.texture.atlas.bitmap.SVGBitmapTextureAtlasTextureRegionFactory;

import org.anddev.andengine.opengl.texture.TextureOptions;
import org.anddev.andengine.opengl.texture.atlas.bitmap.BitmapTextureAtlas;
import org.anddev.andengine.opengl.texture.atlas.bitmap.BuildableBitmapTextureAtlas;
import org.anddev.andengine.opengl.texture.atlas.bitmap.source.IBitmapTextureAtlasSource;
import org.anddev.andengine.opengl.texture.atlas.buildable.builder.BlackPawnTextureBuilder;
import org.anddev.andengine.opengl.texture.atlas.buildable.builder.ITextureBuilder.TextureAtlasSourcePackingException;
import org.anddev.andengine.opengl.texture.region.BaseTextureRegion;
import org.anddev.andengine.opengl.texture.region.TextureRegion;
import org.anddev.andengine.opengl.texture.region.TiledTextureRegion;

import org.anddev.andengine.entity.particle.emitter.CircleOutlineParticleEmitter;
import org.anddev.andengine.entity.particle.initializer.AlphaInitializer;
import org.anddev.andengine.entity.particle.initializer.ColorInitializer;
import org.anddev.andengine.entity.particle.initializer.RotationInitializer;
import org.anddev.andengine.entity.particle.initializer.VelocityInitializer;
//import org.anddev.andengine.entity.particle.modifier._


import org.anddev.andengine.entity.modifier.AlphaModifier

import org.anddev.andengine.opengl.font.Font;
import org.anddev.andengine.opengl.font.FontFactory;
import org.anddev.andengine.ui.activity.BaseGameActivity
import org.anddev.andengine.entity.primitive.Rectangle
import org.anddev.andengine.entity.modifier.PathModifier
import org.anddev.andengine.entity.modifier.PathModifier.Path
import org.anddev.andengine.entity.modifier.PathModifier.IPathModifierListener
import org.anddev.andengine.util.modifier.ease.EaseSineInOut
import org.anddev.andengine.util.Debug

import org.anddev.andengine.entity.scene.menu.MenuScene;
import org.anddev.andengine.entity.scene.menu.MenuScene.IOnMenuItemClickListener;
import org.anddev.andengine.entity.scene.menu.item.IMenuItem;
import org.anddev.andengine.entity.scene.menu.item.TextMenuItem;
import org.anddev.andengine.entity.scene.menu.item.decorator.ColorMenuItemDecorator;
import org.anddev.andengine.util.HorizontalAlign;

import android.graphics.Color;
import android.view.KeyEvent;

import android.content.Context


class MainActivity extends BaseGameActivity with IPinchZoomDetectorListener with IClickDetectorListener with IScrollDetectorListener with IOnSceneTouchListener with IOnMenuItemClickListener with Interface {


  val TAG = "AndEngineTEst"

 
  
  var scene:Scene = _
  var mCamera:ZoomCamera = _

  var mFontTexture:BitmapTextureAtlas=_
  var mFontTexture2:BitmapTextureAtlas=_

  var mBuildableBitmapTextureAtlasTerrain:BuildableBitmapTextureAtlas=_
  var mBuildableBitmapTextureAtlasUnite:BuildableBitmapTextureAtlas=_


  val caseTextureRegion:Map[Environnement[Mode],TextureRegion] = Map()
  val unitesTextureRegion:Array[Array[TextureRegion]] = Array.fill(10)(null)
  val batTextureRegion:Array[Array[TextureRegion]] = Array.fill(10)(null)

  var mBattleT:TextureRegion=_
  var mPinchZoomStartedCameraZoomFactor:Float=_
  var mPinchZoomDetector:PinchZoomDetector=_
  var mScrollDetector:SurfaceScrollDetector=_
  var mClickDetector:ClickDetector = _
  var hud:SuperHUD=_
  val classm = this
  var cartest:Carte = _
  var partie:Partie=_

  def carte = partie.carte

  var mFont:Font=_
  var fontO:Font=_
  var mMenuScene:MenuScene=_


  val TAILLE_CASE = 150

  val (j1,j2,j3,j4) = (Joueur("Ruben"),Joueur("Nicolas"),Joueur("Agustine"),Joueur("Mario"))  
  Joueur.id = -1


  override  def onLoadEngine() = {

    val carteg = CarteTetra.readCarte(CarteTetra.readFile(getApplicationContext().getAssets().open("Carte.txt")))

    val (f1,f2,f3) = (new Fantassin(j1.id), new Fantassin(j2.id),new Fantassin(j3.id))
    val (c1,c2,c3) = (carteg((0,0)),carteg((8,3)),carteg((1,1)))  
    cartest = carteg.factory(carteg.update(List((c1,Some(f1)),(c2,Some(f2)),(c3,Some(f3)))))

    partie = new Partie(cartest,List(j1,j2,j3),Param(1000),this)



    val d = getWindowManager().getDefaultDisplay()
    val CAMERA_WIDTH = d.getWidth()
    val CAMERA_HEIGHT = d.getHeight()

    mCamera = new ZoomCamera(0, 0, CAMERA_WIDTH, CAMERA_HEIGHT);
    mCamera.setBounds(-100, carte.largeur*TAILLE_CASE+100,-100, carte.hauteur*TAILLE_CASE+100);
    mCamera.setBoundsEnabled(true);
    mCamera.offsetCenter(1,1)

    val engineoption = new EngineOptions(true, ScreenOrientation.LANDSCAPE, new RatioResolutionPolicy(CAMERA_WIDTH, CAMERA_HEIGHT), mCamera)
    // engineoption.getTouchOptions().setRunOnUpdateThread(true)
    new Engine(engineoption)

  }



  override def onLoadResources() {




    mBuildableBitmapTextureAtlasTerrain  = new BuildableBitmapTextureAtlas(1024, 1024, TextureOptions.BILINEAR_PREMULTIPLYALPHA)
    mBuildableBitmapTextureAtlasUnite  = new BuildableBitmapTextureAtlas(1024, 1024, TextureOptions.BILINEAR_PREMULTIPLYALPHA)

    mFontTexture = new BitmapTextureAtlas(512, 512, TextureOptions.BILINEAR_PREMULTIPLYALPHA)
    mFontTexture2 = new BitmapTextureAtlas(256, 256, TextureOptions.BILINEAR_PREMULTIPLYALPHA)

    FontFactory.setAssetBasePath("fonts/")	
    mFont = FontFactory.createFromAsset(mFontTexture, this, "ProstoOne-Regular.ttf", 48, true, Color.WHITE)
    fontO = FontFactory.createFromAsset(mFontTexture2, this, "ProstoOne-Regular.ttf", 30, true, Color.WHITE)

    mEngine.getTextureManager().loadTexture(mFontTexture)
    mEngine.getTextureManager().loadTexture(mFontTexture2)
    mEngine.getFontManager().loadFont(mFont)
    mEngine.getFontManager().loadFont(fontO)

    SVGBitmapTextureAtlasTextureRegionFactory.setAssetBasePath("gfx/")

    val lcases=List((Plaine(),"plaine.svg"),(Foret(),"foret.svg"))
    lcases.foreach( x => {
      caseTextureRegion += ((x._1,cresvg(x._2)))
    })

    def cresvg(svg:String) = SVGBitmapTextureAtlasTextureRegionFactory.createFromAsset(mBuildableBitmapTextureAtlasTerrain, this, svg, TAILLE_CASE+1, TAILLE_CASE+1)



    def createTex(svg:String):Array[TextureRegion] =  {
      def textures(pTo:Int):TextureRegion = {
        val pFrom = Color.argb(0,0,0,0)
        SVGBitmapTextureAtlasTextureRegionFactory.createFromAsset(mBuildableBitmapTextureAtlasUnite, this, svg, TAILLE_CASE+1, TAILLE_CASE+1,new ISVGColorMapper() {
			override def  mapColor(pColor:Integer)= {
                                  if(pColor==pFrom) {
                                    pTo
                                  } else { Color.argb(0, Color.red(pColor), Color.green(pColor),Color.blue(pColor)) }
					/* Swap blue and green channel. */
					
			}
	}
		)}
      def idToC(x:Int)= List(Color.BLUE,Color.RED,Color.GREEN,Color.YELLOW)(x)
      Array(partie.joueurs map (x => (textures(idToC(x.id)))):_*)
    }
    
    var i=0
    val lunite=List("fantassin.svg")
    lunite foreach (x => {
      i+=1
      unitesTextureRegion(i-1) = createTex(x)
    })
    i=0
    
    val lbat=List("ville.svg")
    lbat foreach (x => {
      i+=1
      batTextureRegion(0) = Array(cresvg(x))
    })

    mBuildableBitmapTextureAtlasTerrain.build(new BlackPawnTextureBuilder[IBitmapTextureAtlasSource, BitmapTextureAtlas](1))
    mBuildableBitmapTextureAtlasUnite.build(new BlackPawnTextureBuilder[IBitmapTextureAtlasSource, BitmapTextureAtlas](1))

    mEngine.getTextureManager().loadTexture(mBuildableBitmapTextureAtlasTerrain)
    mEngine.getTextureManager().loadTexture(mBuildableBitmapTextureAtlasUnite)

  }
  

  override def onLoadScene() = {

    scene = new Scene()
    mEngine.registerUpdateHandler(new FPSLogger())
    scene.setOnSceneTouchListener(this)
    desscaze
    dessunitz
    createMenuScene()


    mScrollDetector = new SurfaceScrollDetector(this);
    if(MultiTouch.isSupportedByAndroidVersion()) {
      try {
        mPinchZoomDetector = new PinchZoomDetector(this)
      } catch {
        case _ => mPinchZoomDetector = null
      }
    } else {
      mPinchZoomDetector = null
    }

    hud = new SuperHUD(mCamera,mFont,fontO,this)
    hud.setJ(partie.joueura)

    scene.setChildScene(hud)
    mClickDetector = new ClickDetector(this)
    scene.setTouchAreaBindingEnabled(true)
    scene.sortChildren()
    scene
    
  }
  override def onKeyDown(pKeyCode:Int,pEvent:KeyEvent) = {
    if(pKeyCode == KeyEvent.KEYCODE_MENU && pEvent.getAction() == KeyEvent.ACTION_DOWN) {
      if(scene.getChildScene() != hud) {
	/* Remove the menu and reset it. */
	mMenuScene.back();
        scene.setChildScene(hud)
      } else {
	/* Attach the menu. */
	scene.setChildScene(mMenuScene, false, true, true);
      }
      true
    } else {
      if(pKeyCode == KeyEvent.KEYCODE_BACK && pEvent.getAction() == KeyEvent.ACTION_DOWN) {
        effacer
        true
      } else {
        super.onKeyDown(pKeyCode, pEvent)
      }

    }
  }

  override def onMenuItemClicked(pMenuScene:MenuScene, pMenuItem:IMenuItem, pMenuItemLocalX:Float, pMenuItemLocalY:Float) = {
    effacer
if (pMenuScene == mMenuScene) {
    pMenuItem.getID() match {
      case x if x == 0 => {
        reset
	/* Remove the menu and reset it. */
	scene.clearChildScene();
        scene.setChildScene(hud)
	this.mMenuScene.reset();
	true
      }
      case x if x == 1 => {
	this.finish()
        true
      }
      case x if x == 2 => {
        next
        scene.clearChildScene();
        scene.setChildScene(hud)
	this.mMenuScene.reset();
        true
      }
      case _ => false
    }

    
} else true
  }

  def createMenuScene() = {
    mMenuScene = new MenuScene(mCamera);

    val nextMenuItem = new ColorMenuItemDecorator(new TextMenuItem(2, this.mFont, "Next"), 1.0f,0.0f,0.0f, 0.0f,0.0f,0.0f);
    nextMenuItem.setBlendFunction(GL10.GL_SRC_ALPHA, GL10.GL_ONE_MINUS_SRC_ALPHA);
    mMenuScene.addMenuItem(nextMenuItem)

    val resetMenuItem = new ColorMenuItemDecorator(new TextMenuItem(0, this.mFont, "Reset"), 1.0f,0.0f,0.0f, 0.0f,0.0f,0.0f);
    resetMenuItem.setBlendFunction(GL10.GL_SRC_ALPHA, GL10.GL_ONE_MINUS_SRC_ALPHA);
    mMenuScene.addMenuItem(resetMenuItem);


    val quitMenuItem = new ColorMenuItemDecorator(new TextMenuItem(1, this.mFont, "Quit"), 1.0f,0.0f,0.0f, 0.0f,0.0f,0.0f);
    quitMenuItem.setBlendFunction(GL10.GL_SRC_ALPHA, GL10.GL_ONE_MINUS_SRC_ALPHA);
    mMenuScene.addMenuItem(quitMenuItem);

    mMenuScene.buildAnimations();

    mMenuScene.setBackgroundEnabled(false);
    
    mMenuScene.setOnMenuItemClickListener(this);

  }

  override def onScroll(pScrollDetector:ScrollDetector,pTouchEvent:TouchEvent,pDistanceX:Float,pDistanceY:Float) = {
    var zoomFactor = mCamera.getZoomFactor();
    mCamera.offsetCenter(-pDistanceX / zoomFactor, -pDistanceY / zoomFactor)

  }




  override def  onPinchZoomStarted(pPinchZoomDetector:PinchZoomDetector, pTouchEvent:TouchEvent) {
    mPinchZoomStartedCameraZoomFactor = mCamera.getZoomFactor();
  }

  override def  onPinchZoom(pPinchZoomDetector:PinchZoomDetector, pTouchEvent:TouchEvent, pZoomFactor:Float) {
    mCamera.setZoomFactor(mPinchZoomStartedCameraZoomFactor * pZoomFactor)
  }

  
  override def  onPinchZoomFinished(pPinchZoomDetector:PinchZoomDetector, pTouchEvent:TouchEvent, pZoomFactor:Float) {
    mCamera.setZoomFactor(mPinchZoomStartedCameraZoomFactor * pZoomFactor);
  }

  override def onClick(pClickDetector:ClickDetector,pTouchEvent:TouchEvent){
    val (x,y)= (pTouchEvent.getX(),pTouchEvent.getY())
    val maybecaz = CoToCaz(x,y)
    maybecaz.foreach(x => {
      if (casToS.contains(x)) {
        if (!rectDep.get(x).exists(_.head.getAlpha() != 0)) {
          effacer
        }
        inMove(x)
      }
      else {        
       if (!(rectDep contains x)) effacer
        maybecaz.foreach(w => w.dessus filter (y => y match {
          case x:Usine => true
          case _ => false }) foreach (z => {
          construire(w,z)
        }))                                
      }
    })

    
  }



  override def  onSceneTouchEvent(pScene:Scene, pSceneTouchEvent:TouchEvent)= {
    if(this.mPinchZoomDetector != null) {
      mPinchZoomDetector.onTouchEvent(pSceneTouchEvent)


      if(mPinchZoomDetector.isZooming()) {
        mScrollDetector.setEnabled(false)

      } else {
        if(pSceneTouchEvent.isActionDown()) {
          mScrollDetector.setEnabled(true)
        }
        mClickDetector.onTouchEvent(pSceneTouchEvent)
        mScrollDetector.onTouchEvent(pSceneTouchEvent)
      }
    } else {
      mClickDetector.onTouchEvent(pSceneTouchEvent)
      mScrollDetector.onTouchEvent(pSceneTouchEvent)

    }

    true
  }



  override def onLoadComplete() ={ 


  }
  val casToS:Map[Case,(Sprite)]=Map()
  val rectDep:Map[Case,List[Rectangle]] = Map()

  def inMove(caze:Case) = { 
    val sprite = casToS(caze)
    val unite = carte.getUnite(caze)
    if (unite.isDefined && !unite.get.acombattu) {
      val actions = carte.EveryAction(caze).get

      if (rectDep contains caze)
        {
          rectDep(caze) foreach(x => {
            if(x.getAlpha != 0.5f){
            x.registerEntityModifier(new  AlphaModifier(0.5f, 0,0.5f))
            }
            scene.registerTouchArea(x)})
        } else {
          actions._1 foreach ( x => {
            val rectangle = new Rectangle(0, 0, TAILLE_CASE, TAILLE_CASE) {			
	      override def onAreaTouched( pSceneTouchEvent:TouchEvent, pTouchAreaLocalX:Float, pTouchAreaLocalY:Float) = {
                if(pSceneTouchEvent.getAction() == TouchEvent.ACTION_UP){
                  effacer;
                   if (partie.isTour(unite.get.joueur)) {
                     eval(x._2,unite.get.joueur,true)

	           }}
                true
              }
	    }
            
            rectDep.get(caze) match {
              case Some(x) => rectDep += ((caze, rectangle :: x))
              case None => rectDep +=((caze,List(rectangle)))
            }

            rectangle.setZIndex(11)
            rectangle.setBlendFunction(GL10.GL_SRC_ALPHA, GL10.GL_ONE_MINUS_SRC_ALPHA)
            rectangle.setColor(0.087f, 0.149f, 0.255f, 0)
            rectangle.setPosition(x._1.coord._1*TAILLE_CASE,x._1.coord._2*TAILLE_CASE)
            scene.registerTouchArea(rectangle)
            scene.attachChild(rectangle)

          })

          if (!partie.isTour(unite.get.joueur)) {
          actions._2.foreach(x => {
            val minirec = new Rectangle(0,0, TAILLE_CASE/3,TAILLE_CASE/3)
            rectDep.get(caze) match {
              case Some(x) => rectDep += ((caze, minirec :: x))
              case None => rectDep +=((caze,List(minirec)))
            }
            minirec.setZIndex(15)
            minirec.setColor(0.062f,0.149f,0.255f,0.0f)
            minirec.setPosition((x.coord._1.toFloat+1/3f)*TAILLE_CASE,(x.coord._2+1/3f)*TAILLE_CASE)
            scene.attachChild(minirec)
          })}


          rectDep.get(caze) foreach (_ foreach(x =>{x.registerEntityModifier(new  AlphaModifier(0.5f, 0,0.5f))}))
        }


    }
  }

  
  
  def eval(x:List[Action],idj:Int,b:Boolean)={
    partie eval (partie.joueur(idj),x.head)
    ppPopUp(x.tail,idj)
  }


  def ppPopUp(x:List[Action],j:Int) = {
    if (!x.isEmpty){
      hud.addO(x,j)
    }  
  }

  def construire(x:Case,b:Batiment)={}

  def desaU(x:Case) = casToS(x).setAlpha(0.3f)

  def produire(x:Case,y:Unite) = ()

  def capturer(x:Case){

    val ncase=  carte.getCase(x.coord).get
    casToS += ((ncase,casToS(x)))
    casToS -= x
    desaU(ncase)

    batToS += ((ncase,batToS(x)))
    batToS -= x

    val pdv = ncase.dessus.get.asInstanceOf[Capturable].captur
    batToS(ncase).setAlpha((pdv/20.toFloat))

  }

  def joindre(x:PorMov)=()

  def attaque(x:Case,y:Case) = {
    desaU(x)
    val (lu,ld) = List(x,y).span(w => carte.getUnite(w).isDefined) 
    ld foreach (z => {
      effacersprite(casToS(z))
      casToS -= z
    })
    Debug.d(carte.unitz.toString)
    lu foreach (z => casToS(z).getLastChild().asInstanceOf[ChangeableText].setText(carte.getUnite(z).get.pdv.toString))
  }   
                                                   
  def deplacer(to:PorMov) = {
    val sprite = casToS(to.caz)
      sprite.registerEntityModifier(new PathModifier(0.3f,deplacement(sprite,to.path),null,new IPathModifierListener(){

        override def onPathStarted(pPathModifier:PathModifier,pEntity:IEntity) = {}

        override def onPathWaypointStarted(pPathModifier:PathModifier,pEntity:IEntity,pWaypointIndex:Int) = {}
        
        override def onPathWaypointFinished(pPathModifier:PathModifier,pEntity:IEntity,pWaypointIndex:Int) = {}

        override def onPathFinished(pPathModifier:PathModifier,pEntity:IEntity) = {
          casToS -= to.caz
          casToS += ((to.cazf,sprite))
          rectDep -= to.caz
          val (x,y) = cazToCo(to.cazf)
          sprite.setPosition(x,y)
        }
      }))}

  def cazToCo(caze:Case):(Float,Float)=
    ((caze.coord._1*TAILLE_CASE).toFloat,(caze.coord._2*TAILLE_CASE).toFloat)

  def CoToCaz(caze:(Float,Float)):Option[Case]=
    partie.carte.getCase((caze._1/TAILLE_CASE).toInt,(caze._2/TAILLE_CASE).toInt)
  
  def deplacement(sprite:Sprite,path:List[Case]) = path.foldLeft(new Path(path.length))( (acc,caze) =>{
    val (x,y) = cazToCo(caze)
    acc.to(x,y)
  })



  def setPdv(z:Case) =
    casToS(z).getLastChild().asInstanceOf[ChangeableText].setText(carte.getUnite(z).get.pdv.toString)
  
  def next = {
    partie.next
    casToS.foreach(_._2.setAlpha(1.0f))
    partie.joueura.batiz(carte.unitz).foreach(x => setPdv(x._1))
    hud.del
    effacer
    rectDep.clear    
    hud.setJ(partie.joueura)
  }
  


  def effacer = rectDep foreach (_._2 foreach (x => {
    if (x.getAlpha() != 0) {
      x.registerEntityModifier(new  AlphaModifier(0.5f, 0.5f,0))
    }
    scene.unregisterTouchArea(x) }))

               
  def desscaze = carte.carteflat foreach ( x => {caseToSprite(x); x.dessus foreach(y => dessbat(x))})

  def reset = {
    partie.reset
    hud.setJ(partie.joueura)
    dessunitz
  }

  def effacerunitz = { 
    casToS.values foreach ( x => { 
      effacersprite(x);
    })
                
    rectDep.values foreach ( x => x foreach (y => effacersprite(y.asInstanceOf[IEntity])))

    rectDep.clear
    casToS.clear
  }

def effacersprite(y:IEntity) = {
//  scene.unregisterTouchArea(y)
                                                   this.runOnUpdateThread(new Runnable() {
                                                     override def run() {
                                                       scene.detachChild(y)
                                                     }})}

                                                                 

def dessunitz:Unit = {
  effacerunitz
    carte.getUnitz foreach (x => {
      var sprite = new Sprite(x._1.coord._1*TAILLE_CASE,x._1.coord._2*TAILLE_CASE,uniteToSprite(x._2))
      val pdv = new ChangeableText(0,0,mFont,x._2.pdv.toString(),3)
      pdv.setZIndex(15)
      sprite.attachChild(pdv)
      casToS += ((x._1,(sprite)))  
      sprite.setZIndex(10)
      if (x._2.acombattu) sprite.setAlpha(0.5f)

      scene.registerTouchArea(sprite)    
      scene.attachChild(sprite)
    })}

val batToS:Map[Case,Sprite] = Map()
def dessbat(caz:Case) = 
{
  val sprite =  new Sprite(caz.coord._1*TAILLE_CASE,caz.coord._2*TAILLE_CASE,bats(caz.dessus.get))
  batToS += ((caz,sprite))
  sprite.setZIndex(7)
  scene.attachChild(sprite)
  
  
}

def bats(bat:Batiment) = bat match {
  case x:Ville => batTextureRegion(0)(0)
}



  def uniteToSprite(unite:Unite):TextureRegion = unitesTextureRegion(unite.id-1)(unite.joueur)

  
  def caseToSprite(caze:Case)= {
    val coord = caze.coord
    val sprite = new Sprite(coord._1*TAILLE_CASE, coord._2*TAILLE_CASE, caseTextureRegion(caze.typ))    
    sprite.setZIndex(-10)
    scene.attachChild(sprite)
    sprite
  }

}


