object pla extends App {
    def looper(count: Int,loop: Int,streak: Int,w: Seq[Double],vectors: Seq[Seq[Double]],signs: Seq[Int]): Seq[Double] = {
        (count == 100000) || (streak == 100) match {
            case true => w // terminate recursion
            case _ => // recurse
                val i= if(loop > 99) 0 else loop
                val cur_vec = vectors(i)
                val cur_sign = signs(i)
                val check = sign_check(cur_vec,w)
                val W = plus(w,(cur_vec ++ Seq(1.0)).map{j =>.01 * j}.toSeq)
                looper(count+1,i+1,if (check==cur_sign) streak + 1 else 0, if (check==cur_sign) w else W,vectors,signs)
        }
    }

    def dot(a:Seq[Double],b:Seq[Double]) = a.zip(b).map { case (c,d) => c * d }.sum
    def plus(a:Seq[Double],b:Seq[Double]) = a.zip(b).map { case (c,d) => c + d }
    def sign_check(x: Seq[Double], y: Seq[Double]) = math.signum(dot(y,x ++ Seq(1.0))).toInt
    
    val coef: Seq[Double] = Seq(2,3,4,-7)
    val vectors = (0 until 100).map{i => Seq.fill(3)(-9.4 + 20* util.Random.nextDouble())}.toSeq
    val init = vectors.map{v => sign_check(v,coef)}.toSeq
    val random_weights = Seq.fill(4)(-9 + 20* util.Random.nextDouble())
    println(looper(0,0,0,random_weights,vectors,init))
}
