import java.awt.image.{BufferedImage, WritableRaster}
import javax.imageio.ImageIO
import java.io.File
import scala.collection.JavaConversions._
import org.apache.commons.math3.ml.clustering.{KMeansPlusPlusClusterer, DoublePoint}
import org.apache.commons.math3.ml.distance.EuclideanDistance
import org.apache.commons.math3.stat.descriptive.moment.{Mean, Variance, Skewness}

/* 
compute a unique signature ( aka hashcode ) per image (png/jpg/gif)
If image A => sig A
  image B = brighter version of image A
  image C = darker version of A
  image D = version of A with different resolution
We'd like sig B, C, D to be "close" to sig A
Close ? Want SMALL Euclidean distance between hashcodes
This hashcode is a simple vector in R-36.
To run: 
scala -cp commonsmath.jar:. png foo.png foocopy.png
This will read foo.png, compute & print its R-36 signature
and create a foocopy.png with an 8 color palette.
*/


object cbir_new extends App {
    //Calculate image sigs for all images
    val image_sigs = (0 to 9907).map{i => image_stats("image.vary.jpg/" + i + ".jpg")}.toArray

    //Normalize all image sigs
    //For each 36 values in R-36 signature, take (value - mean(value)) / SD(value)
    //This is done to account for the fact that Variances are way higher than skewnesses                             
    val image_sigs_normal = normalize(image_sigs)

    //Get the closest n images to a specified image
    //Done by taking Eudclidean distance of all normalized image sigs
    val closest_n = get_closest(847,10,image_sigs_normal)
    println(closest_n)

    def get_closest(pic_num: Integer,closest_n: Integer,arr: Array[Array[Double]]) = {
        val euclidean = new EuclideanDistance()
        val distances = (0 to 9907).map{i => (i,euclidean.compute(arr(pic_num),arr(i)))}
        distances.sortBy(_._2).drop(1).take(closest_n)
    }

    def normalize(arr: Array[Array[Double]]) = {
        val st = (0 until 36).map{i => stats(arr(i))}.flatten
        val sigs_normal = arr.map{i => 
                                (0 until 36).map{j => (i(j) - st(3 * j)) / st(3 * j + 1)}.toArray}.toArray
        sigs_normal
    }


    def stats(x:Array[Double]) = {
		    List(new Mean().evaluate(x, 0, x.size),
		    new Variance().evaluate(x, 0, x.size),
		    new Skewness().evaluate(x, 0, x.size))
	    }

    def image_stats(x: String): Array[Double] = {
	    // first 3 moments
	

	    //val (imgtype, imagefile, copyfile) = (args(0), args(1), args(2))

	    // read an image & get its raster
	    val img = ImageIO.read(new File(x))
	    val raster:WritableRaster = img.getRaster
	    val (w,h) = (img.getWidth, img.getHeight)



	    // extract all the colors again from the updated raster
	    val data:Seq[Array[Double]] = (0 until w).map { x=>
		    (0 until h).map { y=>
			    val arr = Array.fill[Double](3)(0.0)
			    raster.getPixel(x,y, arr)
			    arr
		    }
	    }.flatten.toSeq
        

	    // compute signature
	    val n = data.size
	    val sig = data
	    .grouped(n/4)
	    .map{ gp:Seq[Array[Double]] =>
		    stats(gp.map{ x=> x(0)}.toArray) ++ // B
		    stats(gp.map{ x=> x(1)}.toArray)  ++ // G
		    stats(gp.map{ x=> x(2)}.toArray) // R
	    }
        //println(sig)
        val sig1 = sig.flatten.toArray
        //println(sig1)
        sig1
    }
}
