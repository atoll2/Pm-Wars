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
import org.anddev.andengine.entity.text.Text;
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

class MainActivity extends BaseGameActivity with IPinchZoomDetectorListener with IScrollDetectorListener with IOnSceneTouchListener with IOnMenuItemClickListener {


  val TAG = "AndEngineTEst"

  val carteg = CarteTetra.readCarte(CarteTetra.readFile("carte.txt"))

  val (j1,j2,j3) = (Joueur(),Joueur(),Joueur())  
  val (f1,f2,f3) = (new Fantassin(j1.id), new Fantassin(j2.id),new Fantassin(j3.id))
  val (f,c) = (new Fantassin(j1.id),carteg((2,1)))
  val chem = carteg((1,0))
  val (c1,c2,c3) = (carteg((0,0)),carteg((8,3)),carteg((2,1)))        
  val carte = carteg.factory(carteg.update(List((c1,Some(f1)),(c2,Some(f2)),(c3,Some(f3)),(c,Some(f)))))
  val partie = new Partie(carte,List(j1,j2,j3),Param(1000))
//  partie.start
  
  var scene:Scene = _
  var mCamera:ZoomCamera = _

  var mFontTexture:BitmapTextureAtlas=_

  var mBuildableBitmapTextureAtlasTerrain:BuildableBitmapTextureAtlas=_
  var mBuildableBitmapTextureAtlasUnite:BuildableBitmapTextureAtlas=_


  var plaineTextureRegion:BaseTextureRegion = _
  var foretTextureRegion:BaseTextureRegion = _
  val fantassinTextureRegion:Map[Int,BaseTextureRegion] = Map()

  var mPinchZoomStartedCameraZoomFactor:Float=_
  var mPinchZoomDetector:PinchZoomDetector=_
  var mScrollDetector:SurfaceScrollDetector=_

  val classm = this


  
  var mFont:Font=_
  var mMenuScene:MenuScene=_

  val TAILLE_CASE = 150

  
  override  def onLoadEngine() = {
    
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

    mFontTexture = new BitmapTextureAtlas(256, 256, TextureOptions.BILINEAR_PREMULTIPLYALPHA);

    FontFactory.setAssetBasePath("fonts/")	
    mFont = FontFactory.createFromAsset(mFontTexture, this, "ProstoOne-Regular.ttf", 48, true, Color.WHITE)
    mEngine.getTextureManager().loadTexture(mFontTexture)
    mEngine.getFontManager().loadFont(mFont)
    SVGBitmapTextureAtlasTextureRegionFactory.setAssetBasePath("gfx/");
    plaineTextureRegion=  SVGBitmapTextureAtlasTextureRegionFactory.createFromAsset(mBuildableBitmapTextureAtlasTerrain, this, "plaine.svg", TAILLE_CASE+1, TAILLE_CASE+1)
    foretTextureRegion = SVGBitmapTextureAtlasTextureRegionFactory.createFromAsset(mBuildableBitmapTextureAtlasTerrain, this, "foret.svg", TAILLE_CASE+1, TAILLE_CASE+1)
    def createTex(svg:String):List[(Int,BaseTextureRegion)] =  {
      def textures(pTo:Int):BaseTextureRegion = {
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
      partie.joueurs map (x => (x.id,textures(idToC(x.id))))
    }
    fantassinTextureRegion ++= createTex("fantassin.svg")
    Debug.d(fantassinTextureRegion.toString)

    

    		try {
			mBuildableBitmapTextureAtlasTerrain.build(new BlackPawnTextureBuilder[IBitmapTextureAtlasSource, BitmapTextureAtlas](1));
			mBuildableBitmapTextureAtlasUnite.build(new BlackPawnTextureBuilder[IBitmapTextureAtlasSource, BitmapTextureAtlas](1));
		} catch  {
                  case x => Debug.e(x);
			
		}

    mEngine.getTextureManager().loadTexture(mBuildableBitmapTextureAtlasTerrain)
    mEngine.getTextureManager().loadTexture(mBuildableBitmapTextureAtlasUnite)

  }
  

  override def onLoadScene() = {
    scene = new Scene()
    mEngine.registerUpdateHandler(new FPSLogger())
    scene.setOnSceneTouchListener(this)
    desscaze
    dessunitz
    mMenuScene = createMenuScene()
    scene.setTouchAreaBindingEnabled(true)
    scene.sortChildren()
    scene
    
  }
  override def onKeyDown(pKeyCode:Int,pEvent:KeyEvent) = {
    if(pKeyCode == KeyEvent.KEYCODE_MENU && pEvent.getAction() == KeyEvent.ACTION_DOWN) {
      if(scene.hasChildScene()) {
	/* Remove the menu and reset it. */
	mMenuScene.back();
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
    pMenuItem.getID() match {
      case x if x == 0 => {
	/* Restart the animation. */
        

        reset
	/* Remove the menu and reset it. */
	scene.clearChildScene();
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
	this.mMenuScene.reset();
        true
      }
      case _ => false

    }
  }

  def createMenuScene():MenuScene = {
    val menuScene = new MenuScene(mCamera);

    val nextMenuItem = new ColorMenuItemDecorator(new TextMenuItem(2, this.mFont, "Next"), 1.0f,0.0f,0.0f, 0.0f,0.0f,0.0f);
    nextMenuItem.setBlendFunction(GL10.GL_SRC_ALPHA, GL10.GL_ONE_MINUS_SRC_ALPHA);
    menuScene.addMenuItem(nextMenuItem)

    val resetMenuItem = new ColorMenuItemDecorator(new TextMenuItem(0, this.mFont, "Reset"), 1.0f,0.0f,0.0f, 0.0f,0.0f,0.0f);
    resetMenuItem.setBlendFunction(GL10.GL_SRC_ALPHA, GL10.GL_ONE_MINUS_SRC_ALPHA);
    menuScene.addMenuItem(resetMenuItem);


    val quitMenuItem = new ColorMenuItemDecorator(new TextMenuItem(1, this.mFont, "Quit"), 1.0f,0.0f,0.0f, 0.0f,0.0f,0.0f);
    quitMenuItem.setBlendFunction(GL10.GL_SRC_ALPHA, GL10.GL_ONE_MINUS_SRC_ALPHA);
    menuScene.addMenuItem(quitMenuItem);

    menuScene.buildAnimations();

    menuScene.setBackgroundEnabled(false);
    
    menuScene.setOnMenuItemClickListener(this);
    menuScene
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



  override def  onSceneTouchEvent(pScene:Scene, pSceneTouchEvent:TouchEvent)= {
    if(this.mPinchZoomDetector != null) {
      mPinchZoomDetector.onTouchEvent(pSceneTouchEvent)

      if(mPinchZoomDetector.isZooming()) {
        mScrollDetector.setEnabled(false)
      } else {
        if(pSceneTouchEvent.isActionDown()) {
          mScrollDetector.setEnabled(true)
        }
        mScrollDetector.onTouchEvent(pSceneTouchEvent)
      }
    } else {
      mScrollDetector.onTouchEvent(pSceneTouchEvent)
    }

    true
  }

  override def onLoadComplete() ={ 


  }
  val casToS:Map[Case,(Sprite,Text)]=Map()
  val rectDep:Map[Case,List[Rectangle]] = Map()

  def inMove(caze:Case) = { 
    val sprite = casToS(caze)._1
    val unite = carte.getUnite(caze)
    if (unite.isDefined) {
      val inMo = carte.inMove(caze)
      inMo foreach (x=> {Debug.d(x.toString);scene.unregisterTouchArea(casToS(x.caz)._1)})
      if (rectDep contains caze)
        {
          rectDep(caze) foreach(x => { x.setVisible(true);scene.registerTouchArea(x)})
        } else {
          inMo foreach ( x => {
            
            val rectangle = new Rectangle(0, 0, TAILLE_CASE, TAILLE_CASE) {			
	      override def onAreaTouched( pSceneTouchEvent:TouchEvent, pTouchAreaLocalX:Float, pTouchAreaLocalY:Float) = {
                if(pSceneTouchEvent.isActionDown()) {
                  Debug.d(sprite.toString())
                  if (partie.isTour(unite.get.joueur)) deplacer(partie.joueur(unite.get.joueur),x,sprite)
                                                          

	        }
                true
              }
	    }
            
            rectDep.get(caze) match {
              case Some(x) => rectDep += ((caze, rectangle :: x))
              case None => rectDep +=((caze,List(rectangle)))
            }
            rectangle.setZIndex(10)
            rectangle.setColor(0.087f, 0.149f, 0.255f, 0.5f)
            rectangle.setPosition(x.caz.coord._1*TAILLE_CASE,x.caz.coord._2*TAILLE_CASE)
            scene.registerTouchArea(rectangle)
            scene.attachChild(rectangle)
          })


        }
    }

  }
  def deplacer(joueur:Joueur,to:PorMov,sprite:Sprite) = {
    partie ! ((joueur,Deplacement(CheckedL(to))))
      sprite.registerEntityModifier(new PathModifier(0.3f,deplacement(sprite,to.path),null,new IPathModifierListener(){
        override def onPathStarted(pPathModifier:PathModifier,pEntity:IEntity) = {effacer}
        override def onPathWaypointStarted(pPathModifier:PathModifier,pEntity:IEntity,pWaypointIndex:Int) = {}
        override def onPathWaypointFinished(pPathModifier:PathModifier,pEntity:IEntity,pWaypointIndex:Int) = {}
        override def onPathFinished(pPathModifier:PathModifier,pEntity:IEntity) = {dessunitz}
      }))}
  
  def deplacement(sprite:Sprite,path:List[Case]) = path.foldLeft(new Path(path.length))( (acc,caze) =>{Debug.d(caze.coord.toString()); acc.to(caze.coord._1*TAILLE_CASE,caze.coord._2*TAILLE_CASE)})

  def next = {
    partie.next
    dessunitz
  }
  
  def effacer = rectDep map (_._2 foreach (x => {x.setVisible(false);scene.unregisterTouchArea(x)}))

               
  def desscaze = carte.carteflat map ( x => caseToSprite(x)) 
  def reset = {
    partie.reset
    dessunitz
  }
  def effacerunitz = { casToS.values foreach ( x => { effacersprite(x._1);effacersprite(x._2)})
                
    rectDep.values foreach ( x => x foreach (y => {scene.unregisterTouchArea(y)
                                                   this.runOnUpdateThread(new Runnable() {
                                                     override def run() {
                                                       scene.detachChild(y)
                                                     }
                })

                                                
                                                 }))

                      rectDep.clear
                      casToS.clear
}

def effacersprite(y:Sprite) = {scene.unregisterTouchArea(y)
                                                   this.runOnUpdateThread(new Runnable() {
                                                     override def run() {
                                                       scene.detachChild(y)
                                                     }})}

def effacersprite(y:Text) = {scene.unregisterTouchArea(y)
                                                   this.runOnUpdateThread(new Runnable() {
                                                     override def run() {
                                                       scene.detachChild(y)
                                                     }})}                                                                   

def dessunitz:Unit = {

  effacerunitz
    carte.getUnitz foreach (x => {
      var sprite = new Sprite(x._1.coord._1*TAILLE_CASE,x._1.coord._2*TAILLE_CASE,uniteToSprite(x._2))
      {			
	override def onAreaTouched( pSceneTouchEvent:TouchEvent, pTouchAreaLocalX:Float, pTouchAreaLocalY:Float) = {
          if(pSceneTouchEvent.isActionDown()) {
            effacer 
   
            inMove(x._1)
	    
          }
          true
	}
      }

      var pdv = new Text(x._1.coord._1*TAILLE_CASE+25,x._1.coord._2*TAILLE_CASE+25,mFont,x._2.pdv.toString(),HorizontalAlign.CENTER)
      pdv.setZIndex(15)

      casToS += ((x._1,(sprite,pdv)))  
      sprite.setZIndex(5)
      if (x._2.acombattu) sprite.setAlpha(0.5f)

      scene.registerTouchArea(sprite)    
      scene.attachChild(pdv)
      scene.attachChild(sprite)
      sprite
    })}

  def texToSprite(caze:Case):TextureRegion = caze.typ match {
    case Plaine() => plaineTextureRegion match { case x:TextureRegion => x }
    case Foret() => foretTextureRegion match { case x:TextureRegion => x }
  }    
  def uniteToSprite(unite:Unite):TextureRegion = {
    unite match {
      case x:Fantassin => fantassinTextureRegion(unite.joueur) match { case x:TextureRegion => x }
    }

  }
  def caseToSprite(caze:Case)= {
    val coord = caze.coord
    val sprite = new Sprite(coord._1*TAILLE_CASE, coord._2*TAILLE_CASE, texToSprite(caze))
   
    

    sprite.setZIndex(-10)
    scene.attachChild(sprite)
    sprite
  }

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
}


