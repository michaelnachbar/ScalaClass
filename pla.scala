import scala.util.Random.nextDouble

object pla extends App {
    def looper(count: Int,loop: Int,streak: Int,w: Seq[Double],vectors: Seq[Seq[Double]],signs: Seq[Int]): Seq[Double] = {
        if (count == 100000)
            w
        else if (streak == 100){
            w
        }
        else {
            val loop_check = if(loop > 99) 0 else loop
            val cur_vec = vectors(loop_check)
            val cur_sign = signs(loop_check)
            val check = sign_check(cur_vec,w)
            if (check == cur_sign){              
                looper(count+1,loop_check+1,streak+1,w,vectors,signs)
            }
            else if (check == -1){
                val new_vector = plus(w,(cur_vec ++ Seq(1.0)).map{i =>.01 * i}.toSeq)
                looper(count+1,loop_check+1,0,new_vector,vectors,signs)
            }
            else{
                val new_vector_1 = plus(w,(cur_vec ++ Seq(1.0)).map{j =>.01 * j}.toSeq)
                looper(count+1,loop_check+1,0,new_vector_1,vectors,signs)
            }
        }
    }
            
            

    def dot(a:Seq[Double],b:Seq[Double]) = a.zip(b).map { case (c,d) => c * d } sum
    def plus(a:Seq[Double],b:Seq[Double]) = a.zip(b).map { case (c,d) => c + d }
    def sign(x: Double) = {if (x == 0) 0 else if (x > 0) 1 else -1}
    def sign_check(x: Seq[Double], y: Seq[Double]) = {sign(dot(y,x ++ Seq(1.0)))}
    
    val coef: Seq[Double] = Seq(2,3,4,-7)
    val vectors = (0 until 100).map{i => Seq.fill(3)(-9.4 + 20*nextDouble())}.toSeq
    val signs = vectors.map{v => sign_check(v,coef)}.toSeq
    val random_weights = Seq.fill(4)(-9 + 20*nextDouble())
    println(looper(0,0,0,random_weights,vectors,signs))
}
