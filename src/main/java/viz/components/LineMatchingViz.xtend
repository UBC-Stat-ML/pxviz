package viz.components

import java.util.ArrayList
import java.util.Map
import processing.core.PApplet
import viz.core.Viz
import viz.core.PublicSize

class LineMatchingViz extends Viz {
  
  val Map<Double, Double> matching
  val double min
  val double max
  
  new(Map<Double,Double> matching, PublicSize publicSize) {
    super(publicSize) 
    this.matching = matching
    val allNumbers = new ArrayList<Double> => [
      addAll(matching.keySet)
      addAll(matching.values)
    ]
    min = allNumbers.min
    max = allNumbers.max
  }
  
  override protected privateSize() {
    return new PrivateSize(1, 1)
  }
  
  private def double xPosition(double number) {
    return (number - min) / (max - min)
  }
  
  override protected draw() {
    strokeWeight(0.0001f)
    colorMode(PApplet::HSB, 1.0f) 
    for (c1 : matching.keySet) {
      val c2 = matching.get(c1)
      if (c2 !== null) {
        val n1 = xPosition(c1)
        val n2 = xPosition(c2)
        stroke(n1 as double as float, 1.0f, 1.0f) 
        line(n1 as double as float, 0.0f, n2 as double as float, 1.0f)
      }
    }
  }
}