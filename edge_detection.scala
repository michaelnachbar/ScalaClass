import java.awt.image.{BufferedImage, WritableRaster}
import javax.imageio.ImageIO
import java.io.File
import scala.collection.JavaConversions._
import java.awt.image.{BufferedImage, WritableRaster}
import javax.imageio.ImageIO
import java.io.File
import scala.math.max

object edge_detection extends App {
    //For a single pixel take the average of the 9 around it
    def smooth_pixel(img_mat: Array[Array[Double]],x: Int,y: Int) = {
        val w = img_mat.size
        val h = img_mat(0).size
        if (!(x>0 & x< w-1 & y>0 & y< h-1))
            img_mat(x)(y)
        else {
            val square = (x-1 to x+1).map {x=>
                (y-1 to y+1).map {y=>
                    img_mat(x)(y)
                }
            }.flatten.toSeq
            (1.0/9) * square.sum
        }
        
        
    }

    //Run smooth_pixel for each pixel
    def smooth(img_mat: Array[Array[Double]]) = {
        val w = img_mat.size
        val h = img_mat(0).size
        val new_mat = (0 until w).map { x=>
		    (0 until h).map { y=>
                    smooth_pixel(img_mat,x,y)
		    }.toArray
	    }.toArray
        println(new_mat(1)(1))
        new_mat 
    }

    //Subtract matricies (convert negatives to 0)
    def subtract_matricies(img_mat_1: Array[Array[Double]],img_mat_2: Array[Array[Double]]) = {
        val w = img_mat_1.size
        val h = img_mat_1(0).size
        val new_mat = (0 until w).map { x=>
		    (0 until h).map { y=>
                    4 * max(0,img_mat_1(x)(y) - img_mat_2(x)(y))
		    }.toArray
	    }.toArray
        new_mat
    }

    //Take a pixel matrix and filename, output a file
    def write_matrix(img_mat: Array[Array[Double]], file_name: String) = {
        val w = img_mat.size
        val h = img_mat(0).size
        val raster:WritableRaster = img.getRaster
	    (0 until w).foreach { x=>
		    (0 until h).foreach { y=>
			    raster.setPixel(x,y,Array(img_mat(x)(y),img_mat(x)(y),img_mat(x)(y)))
		    }
	    }
	    val copy = new BufferedImage(w,h,5)
	    copy.setData(raster)
	    ImageIO.write(copy, "jpg", new File(file_name))
    }

	val imgtype = "jpg"
    val imagefile = "image.vary.jpg/26.jpg"
    val copyfile = "test_image1.jpg"
	val img = ImageIO.read(new File(imagefile))
	val mytype = img.getType
    println(mytype)
	val raster:WritableRaster = img.getRaster
	val (w,h) = (img.getWidth, img.getHeight)

    //Create grayscale matrix
    //I wasn't sure how to do this with colors :-(
	val mat = (0 until w).map { x=>
		(0 until h).map { y=>
			val arr = Array.fill[Double](3)(0.0)
			val bgr = raster.getPixel(x,y, arr)
			val avg = .2989 * arr(2) + .1140 * arr(0) + .587 * arr(1)
            avg
		}.toArray
	}.toArray
    val smooth_mat = smooth(mat)
    write_matrix(smooth_mat,"Smooth_Image.jpg")
    val edge_mat = subtract_matricies(mat,smooth_mat)
    write_matrix(edge_mat,"Edge_Image.jpg")
}
