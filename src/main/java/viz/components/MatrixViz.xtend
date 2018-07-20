package viz.components

import xlinear.Matrix 
import org.eclipse.xtend.lib.annotations.Data
import processing.core.PApplet
import viz.core.PublicSize
import viz.core.Viz.PrivateSize
import viz.core.Viz

@Data class MatrixViz extends Viz  {
  val Matrix m
  val CellFiller filler
  
  new (Matrix m, CellFiller filler, PublicSize size) {
    super(size)
    this.m = m
    this.filler = filler
  }
  
  @FunctionalInterface
  static interface CellFiller {
    def void colour(int row, int col, double value, PApplet result)
  }
  
  // Assumes matrix entries between zero and one
  def static MatrixViz greyScale(Matrix m, PublicSize size) {
    return new MatrixViz(m, greyScale, size)
  }
  
  public static val CellFiller greyScale = [__, ___, v, result | result.fill(((1.0 - v) * 255.0) as int)]
  
  // Assumes matrix entries between zero and one
  static def CellFiller colours(int index, int paletteSize) {
    val float norm = paletteSize
    if (index < 0 || index >= paletteSize)
      throw new RuntimeException
    return [__, ___, v , result | 
      result.colorMode(PApplet::HSB, paletteSize) 
      result.fill(index as float, v as float * norm, norm)  
    ]
  }
  
  override draw() {
    noStroke
    pushStyle
    for (var int r = 0; r < m.nRows; r++) { 
      for (var int c = 0; c < m.nCols; c++) { 
        filler.colour(r, c, m.get(r, c), applet)
        rect(c, r, 1.0f, 1.0f)
      }
    }
    popStyle
  }
  
  override privateSize() { new PrivateSize(m.nCols as float,m.nRows as float) }
}