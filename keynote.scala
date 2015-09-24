import java.awt.image.{BufferedImage, WritableRaster}
import javax.imageio.ImageIO
import java.io.File
import scala.collection.JavaConversions._
import org.apache.commons.math3.ml.clustering.{KMeansPlusPlusClusterer, DoublePoint}
import org.apache.commons.math3.ml.distance.EuclideanDistance
import org.apache.commons.math3.stat.descriptive.moment.{Mean, Variance, Skewness}

import scala.math.pow


object keynote extends App {
    //Calculate image sigs for all images
    val image_sigs = (2 to 3000).map{i => image_stats("images/TF1 - Transition Frame " + i + ".jpg")}.toArray
    //Normalize all image sigs
    //For each 3 values in R-3 signature, take (value - mean(value)) / SD(value)
    //This is done to account for the fact that Variances are way higher than skewnesses                             
    val image_sigs_normal = normalize(image_sigs)
    get_frames(0,1,image_sigs_normal)

    //Scroll through all frames recursively and print all keynotes
    def get_frames(last: Int, curr: Int, images: Array[Array[Double]]){
        if (curr < images.size){
            val euclidean = new EuclideanDistance()
            val distance = euclidean.compute(images(last),images(curr))
            if (distance > 1.5){
                println(curr + 2)
                get_frames(curr,curr+1,images)
            }
            else
                get_frames(last,curr+1,images)
        }
    }

    def normalize(arr: Array[Array[Double]]) = {
        val st = (0 until 3).map{i => stats(arr(i))}.flatten
        val sigs_normal = arr.map{i => 
                                (0 until 3).map{j => (i(j) - st(3 * j)) / pow(st(3 * j + 1),.5)}.toArray}.toArray
        sigs_normal
    }


    def stats(x:Array[Double]) = {
		    List(new Mean().evaluate(x, 0, x.size),
		    new Variance().evaluate(x, 0, x.size),
		    new Skewness().evaluate(x, 0, x.size))
	    }

    def image_stats(x: String): Array[Double] = {
	    val img = ImageIO.read(new File(x))
	    val raster:WritableRaster = img.getRaster
	    val (w,h) = (img.getWidth, img.getHeight)



	    // Convert to grayscale to get the R-3
	    val data:Array[Double] = (0 until w).map { x=>
		    (0 until h).map { y=>
			    val arr = Array.fill[Double](3)(0.0)
			    raster.getPixel(x,y, arr)
			    .2989 * arr(2) + .1140 * arr(0) + .587 * arr(1)
		    }
	    }.flatten.toArray
        

	    // compute signature
	    val sig = stats(data).toArray
        sig
    }
}
