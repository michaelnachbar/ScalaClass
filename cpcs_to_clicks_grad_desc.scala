object cpc_to_clicks_model extends App {

    def error_func(x: Double,y: Double,a: Double,b: Double,c: Double) = {
        math.pow((y - a / (1 + math.exp(c * (b-x)))),2)
    }

    def cpc_to_clicks_grad_desc(x_vals: Seq[Double],y_vals: Seq[Double],a: Double,b: Double,c: Double,step: Int,last_err: Double): Seq[Double] = {
        val min_diff = .00001
        val min_steps = 1000
        val max_steps = 100000
        val learning_rate = .001 / x_vals.length
        val err: Double = x_vals.zip(y_vals).map {case(x,y) => error_func(x,y,a,b,c)}.sum
        //(step >= max_steps) match {
        (step >= max_steps || step > min_steps & err > last_err - min_diff) match {
            case true => Seq(a,b,c,err,step)
            case _ =>
                
                val grad_a: Double = x_vals.zip(y_vals).map {case(x,y) =>
                    -2 * (y - (a/(1 + math.exp((b-x) * c)))) / (1 + math.exp((b-x) * c))}.sum
                val grad_b: Double = x_vals.zip(y_vals).map {case(x,y) =>
                    2 * a * c * math.exp(c*(b-x)) * (y - (a / (1 + math.exp(c*(b-x))))) / math.pow((1 + math.exp(c*(b-x))),2)}.sum
                val grad_c: Double  = x_vals.zip(y_vals).map {case(x,y) =>
                    2 * a * (b-x) * math.exp((b-x)*c) * (y - (a / (1 + math.exp((b-x) * c)))) / math.pow((1 + math.exp((b-x) * c)),2)}.sum
                cpc_to_clicks_grad_desc(x_vals,y_vals,a - grad_a*learning_rate,b - grad_b*learning_rate,c - grad_c*learning_rate,step +1,err)
        }
    }

    val cpcs: Seq[Double] = Seq(
        0.39,
        0.56,
        0,
        0.16,
        0.56,
        0,
        0.27,
        0.55,
        0.48,
        0.72,
        0.35,
        0.66,
        0.24,
        0,
        0.33,
        0.42,
        0.9,
        0.74,
        0.78,
        1.04,
        0.98,
        1.25,
        1.14,
        1.1,
        0.8,
        0.71,
        0.57,
        0.52,
        0.39,
        0.56,
        0,
        0.16,
        0.56,
        0,
        0.27)

    val clicks: Seq[Double] = Seq(
        3,
        3,
        0,
        2,
        4,
        0,
        2,
        1,
        7,
        4,
        1,
        4,
        2,
        0,
        1,
        5,
        6,
        11,
        13,
        23,
        18,
        39,
        34,
        31,
        18,
        19,
        15,
        8,
        3,
        3,
        0,
        2,
        4,
        0,
        2)

    
    println(cpc_to_clicks_grad_desc(cpcs,clicks,39*1.5,1.07,4.3,0,0))



    

}
