package viz.components

import blang.inits.experiments.Experiment
import blang.inits.Arg
import java.io.File
import briefj.BriefIO
import java.util.List
import briefj.BriefMaps
import java.util.TreeMap
import java.util.ArrayList
import java.util.Collections
import viz.components.LineMatchingViz
import viz.core.Viz

class TemperatureMatchings extends Experiment {
  
  @Arg File densities
  
  def trueCost(double t1, double t2) { [double v1, double v2 | 1.0 - Math.min(1.0, Math.exp((t2-t1)*(v2-v1)))] }
  def symCost(double t1, double t2) {  [double v1, double v2 | 1.0 - Math.exp(-(t2-t1)*Math.abs(v2-v1))] }
  
  override run() {
    // read energies
    val energies = new TreeMap<Double, List<Double>>
    for (entry : BriefIO.readLines(densities).indexCSV) {
      val annealParam = Double.parseDouble(entry.get("temperature"))
      val density = Double.parseDouble(entry.get("value"))
      val energy = - density / annealParam
      BriefMaps.getOrPutList(energies, annealParam).add(energy)
    }
    // show optimal transports between consecutive temperatures
    for (useSym : #[true, false]) {
      val annealParams = new ArrayList(energies.keySet)
      Collections::sort(annealParams)
      for (var int i = 0; i < annealParams.size - 1; i++) {
        val t1 = annealParams.get(i)
        val t2 = annealParams.get(i+1) // t1 < t2 
        val pop1 = energies.get(t1)
        val pop2 = energies.get(t2)
        try {
          val match = HungarianAlgorithm::optimalMatching(pop1, pop2, if (useSym) symCost(t1, t2) else trueCost(t1, t2)) 
          val file = results.getFileInResultFolder((if (useSym) "sym-" else "true-") + t1 + "---" + t2 + ".pdf")
          new LineMatchingViz(match, Viz::fixHeight(800)) => [ output(file) ]
        } catch (Exception e) { System.err.println("failed sym?=" + useSym + ", ts=" + t1 + "---" + t2)}
      }
    }
  }
  
  static def void main(String [] args) {
    Experiment.start(args) 
  }
}