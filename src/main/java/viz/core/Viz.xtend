package viz.core

import processing.core.PApplet
import java.io.File
import org.eclipse.xtend.lib.annotations.Data
import blang.inits.experiments.Experiment
import blang.inits.Arg
import blang.inits.DefaultValue
import gifAnimation.GifMaker

abstract class Viz extends Experiment {
  
  /**
   * PrivateSize is used internally by a visualization as its 
   * internal coordinate system. The object specifies both its 
   * height and width based on input data to visualize and specifics 
   * of the visualization.. 
   */
  protected def PrivateSize privateSize()
  
  /**
   * When a viz is used by someone else (either by a main application 
   * that will print the viz to screen/pdf, or by another viz as a child element), 
   * either a height or width needs to be specified, but not both since the ratio 
   * is already determined by the PrivateSize.
   * 
   * Use the static methods fixWidth() and fixHeight() to cleanly create instances 
   * of PublicSize
   */
  val PublicSize publicSize
  
  new (PublicSize publicSize) {
    this.publicSize = publicSize
  }
  
  @Data static class PrivateSize {
    val float width
    val float height
    new (Number width, Number height) {
      this.width = width.floatValue
      this.height = height.floatValue
    }
  }
  
  static def fixHeight(Number height) {
    return new PublicSize(true, height.floatValue)
  }
  
  static def fixWidth(Number width) {
    return new PublicSize(false, width.floatValue)
  }
  
  protected def void draw()
  protected extension var PApplet applet
  
  def float publicWidth() {
    if (publicSize.isHeight) 
      privateSize.width * publicSize.value / privateSize.height
    else 
      publicSize.value
  }
  
  def float publicHeight() {
    if (publicSize.isHeight)
      publicSize.value
    else 
      privateSize.height * publicSize.value / privateSize.width
  }
  
  // Use those to avoid confusion in feature calls
  private def void drawViz() { draw }
  boolean running = false
  
  protected def void addChild(Viz child, Number x, Number y) {
    child.applet = this.applet
    pushMatrix
    pushStyle
    translate(x.floatValue, y.floatValue)
    scale(child.scaleRatio, child.scaleRatio)
    child.drawViz
    popMatrix
    popStyle
  }
  
  def void show() { execute(Mode.SHOW, null) }
  
  def void output(String path) { output(new File(path)) }
  def void output(File output) { execute(Mode.SAVE_PDF, output) }
  
  def void outputAnimatedGif(String path) { outputAnimatedGif(new File(path)) }
  def void outputAnimatedGif(File output) { execute(Mode.SAVE_ANIMATED_GIF, output) }
  
  private def void execute(Mode mode, File output) {
    if (running)
      throw new RuntimeException("Cannot run twice.")
    running = true
    
    applet = new PApplet {
      override settings() {
        if (mode == Mode.SAVE_PDF)
          size(publicWidth as int, publicHeight as int, PDF, output.absolutePath)
        else
          size(publicWidth as int, publicHeight as int)
      }
      override draw() {
        background(255)
        scale(scaleRatio, scaleRatio)
        drawViz()
        if (mode == Mode.SAVE_ANIMATED_GIF)
          saveGifFrame
        if (mode == Mode.SAVE_PDF)
          exit
      }
      override exitActual() {
        if (mode == Mode.SHOW) 
          super.exitActual
      }
    }
    if (mode == Mode.SAVE_ANIMATED_GIF)
      gif = new GifMaker(applet, output.absolutePath) 
    PApplet.runSketch(#[this.class.simpleName.toString], applet) 
  }
  
  
  protected var GifMaker gif = null
  def private saveGifFrame() { // called automatically
    if (gif !== null)
      gif.addFrame
  }
  def finishGif() {
    if (gif !== null) {
      gif.finish
      gif = null
    }
  }
  
  static enum Mode { SHOW, SAVE_PDF, SAVE_ANIMATED_GIF }
  
  @Arg      @DefaultValue("SAVE_PDF")
  public Mode mode = Mode.SAVE_PDF
  override run() {
    if (mode == Mode.SHOW) 
      show()
    else if (mode == Mode.SAVE_PDF)
      output(results.getFileInResultFolder("output.pdf")) 
    else if (mode == Mode.SAVE_ANIMATED_GIF)
      outputAnimatedGif(results.getFileInResultFolder("output.gif")) 
    else
      throw new RuntimeException
  }
  
  private def float scaleRatio() { publicWidth / privateSize.width }
}