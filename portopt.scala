/*
example Auction Curve Modeling
Pick cpc = a * log(b + c * bid)
Params: a,b,c
*/


import org.apache.commons.math3.analysis.ParametricUnivariateFunction
import org.apache.commons.math3.fitting.WeightedObservedPoint
import org.apache.commons.math3.fitting.SimpleCurveFitter
import scala.collection.JavaConversions._

class Bid2CPC extends ParametricUnivariateFunction {
      def value(x:Double, param:Double*):Double = {
          val (a,b,c) = (param(0), param(1), param(2))
          a * math.log(b + c * x)
      }
      def gradient(x:Double, param:Double*):Array[Double] = {
          val (a,b,c) = (param(0), param(1), param(2))
          val grad_a = math.log(b + c * x)
          val grad_b = a/(b+c*x)
          val grad_c = a*x/(b+c*x)
          Array(grad_a, grad_b, grad_c)
      }
      }

object portopt extends App {
    val res0 = new Bid2CPC()


    //val bids = (0.0 to 3.0 by 0.1)
    //val data = bids.map{ x=> (x,res0.value(x, 3,1,0.8))}
    //Use actual bid/cpc data for the keyword "stackable washer dryer"
    val data = Seq((1.46,1.28),
    (1.61,1.26),
    (1.61,1.32),
    (1.61,1.4),
    (1.61,1.38),
    (1.77,1.85),
    (2.14,1.65),
    (2.14,1.8),
    (2.14,1.76),
    (2.14,1.86),
    (1.65,1.48),
    (1.62,1.34),
    (1.62,1.43),
    (1.62,1.0),
    (0.95,1.33),
    (0.55,0.91),
    (0.66,0.63),
    (0.79,0.55),
    (0.95,0.68),
    (1.71,0.82),
    (1.95,1.29),
    (1.95,1.43),
    (1.95,1.6),
    (1.95,1.63),
    (1.95,1.55),
    (1.95,1.53),
    (1.95,1.56),
    (1.25,1.6)
    )


    val points = data.map{ pt => new WeightedObservedPoint(1.0, pt._1, pt._2) }
    val z = SimpleCurveFitter.create(new Bid2CPC(), Array(1,1,1)).withMaxIterations(10000).fit(points)  
    z.foreach(println)
}
