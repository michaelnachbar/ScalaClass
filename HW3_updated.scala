//package akka.tutorial.first.scala

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration.Duration
import scala.concurrent.duration

import scala.math.exp
import scala.math.sqrt
import scala.math.pow
import scala.math.max

import scala.collection.mutable



case class single_stock_info(ticker: String,price: Double,vol: Double,strikes: List[Double])
case class single_strike_info(ticker: String,price: Double,vol: Double,strike: Double)
case class call_info(time: Double, price: Double, strike: Double, rf: Double, sigma: Double, depth: Int)
case class strike_result(ticker: String,strike: Double, value: Double)
case class all_stocks_info(prices: Map[String,Double],sigmas: Map[String,Double],strikes: Map[String,List[Double]])
case class period_before_params(values: IndexedSeq[Double],price: Double,strike: Double,up: Double,p0: Double)

class stock_result(sr: mutable.Buffer[strike_result],ticker: String){
    val strike_map: Map[(String,Double),Double] = sr.map{sr =>
                                                    ((ticker,sr.strike),sr.value)}.toMap
}
    

object HW3_new extends App {
    //Define the variables
    val prices: Map[String,Double] = Map(
            "MMM" -> 143.14,
            "AXP" -> 77.04,
            "AAPL" -> 113.81,
            "BA" -> 131.71,
            "CAT" -> 76.55,
            "CVX" -> 80.71,
            "CSCO" -> 26,
            "KO" -> 39.22,
            "DIS" -> 102.17,
            "DD" -> 51.77,
            "XOM" -> 75.09,
            "GE" -> 24.82,
            "GS" -> 189.74,
            "HD" -> 116.79,
            "IBM" -> 148.00,
            "INTC" -> 29.09,
            "JNJ" -> 94.35,
            "JPM" -> 64.20,
            "MCD" -> 95.30,
            "MRK" -> 54.05,
            "MSFT" -> 43.80,
            "NKE" -> 111.92,
            "PFE" -> 32.19,
            "PG" -> 70.52,
            "TRV" -> 99.49,
            "UTX" -> 92.25,
            "UNH" -> 116.15,
            "VZ" -> 45.90,
            "V" -> 71.18,
            "WMT" -> 64.80
        )
        //Wasn't sure how to calculate these so they are lifted from Namrata's code.
        val sigmas: Map[String,Double] = Map(
            "MMM" -> 0.111014,
            "AXP" -> 0.129384,
            "AAPL" -> 0.166714,
            "BA" -> 0.152693,
            "CAT" -> 0.15169,
            "CVX" -> 0.122724,
            "CSCO" -> 0.141522,
            "KO" -> 0.073676,
            "DIS" -> 0.093929,
            "DD" -> 0.137368,
            "XOM" -> 0.090433,
            "GE" -> 0.097325,
            "GS" -> 0.12121,
            "HD" -> 0.145388,
            "IBM" -> 0.129919,
            "INTC" -> 0.200586,
            "JNJ" -> 0.10655,
            "JPM" -> 0.128487,
            "MCD" -> 0.127921,
            "MRK" -> 0.151818,
            "MSFT" -> 0.169389,
            "NKE" -> 0.114313,
            "PFE" -> 0.086067,
            "PG" -> 0.111913,
            "TRV" -> 0.116392,
            "UTX" -> 0.116216,
            "UNH" -> 0.160558,
            "VZ" -> 0.110171,
            "V" -> 0.159881,
            "WMT" -> 0.18536
        )
        //Strikes for each stock for options for 10/16/2015
        //Some have <20 as <20 were listed on Yahoo Finance
        //e.g. http://finance.yahoo.com/q/op?s=ge&date=1444953600
        val strikes: Map[String,List[Double]] = Map(
            "MMM" -> List(90.0,120.0,125.0,130.0,135.0,140.0,145.0,150.0,155.0,160.0,165.0,170.0,175.0,180.0,185.0,190.0,195.0,200.0,210.0,220.0,230.0,240.0,250.0),
            "AXP" -> List(47.5,55.0,60.0,65.0,67.5,70.0,72.5,75.0,77.5,80.0,82.5,87.5,90.0,95.0,100.0,105.0,110.0,115.0), 
            "AAPL" -> List(60.0,65.0,70.0,75.0,80.0,85.0,90.0,95.0,100.0,105.0,110.0,115.0,120.0,125.0,130.0,135.0,140.0,145.0,150.0,155.0,160.0,165.0,170.0,175.0,180.0,185.0,190.0),
            "BA" -> List(85.0,100.0,105.0,110.0,115.0,120.0,125.0,130.0,135.0,140.0,145.0,150.0,155.0,160.0,165.0,175.0),
            "CAT" -> List(50.0,60.0,65.0,67.5,70.0,72.5,75.0,77.5,80.0,82.5,85.0,87.5,90.0,95.0,100.0,105.0,110.0),
            "CVX" -> List(55.0,60.0,65.0,70.0,75.0,80.0,85.0,90.0,95.0,100.0,105.0,110.0) ,
            "CSCO" -> List(17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,28.0,29.0,30.0,31.0,32.0,33.0,34.0,35.0,36.0,37.0,38.0,39.0,40.0),
            "KO" -> List(35.0,36.0,37.0,38.0,39.0,40.0,41.0,42.0,43.0,44.0,45.0,46.0,47.0),
            "DIS" -> List(70.0,75.0,80.0,85.0,90.0,92.5,95.0,97.5,100.0,105.0,110.0,115.0,120.0,125.0,130.0,135.0,140.0,145.0,150.0),
            "DD" -> List(40.0,45.0,47.5,50.0,52.5,55.0,57.5,60.0,62.5,65.0,67.5,70.0,72.5,75.0,77.5,80.0,82.5,85.0,87.5,90.0,95.0,100.0),
            "XOM" -> List(50.0,55.0,60.0,65.0,67.5,70.0,72.5,75.0,77.5,80.0,82.5,85.0,87.5,90.0,92.5,95.0,97.5,100.0,105.0,110.0,115.0,120.0),
            "GE" -> List(19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,28.0,29.0,30.0,31.0),
            "GS" -> List(120.0,130.0,135.0,140.0,150.0,155.0,160.0,165.0,170.0,175.0,180.0,185.0,190.0,195.0,200.0,205.0,210.0,215.0,220.0,225.0,230.0,235.0,240.0,245.0,250.0,255.0,260.0,265.0,285.0),
            "HD" -> List(80.0,95.0,100.0,105.0,110.0,115.0,120.0,125.0,130.0),
            "IBM" -> List(85.0,100.0,110.0,120.0,130.0,135.0,140.0,145.0,150.0,155.0,160.0,165.0,170.0,175.0,180.0,185.0,190.0,195.0,200.0,205.0,210.0,215.0,220.0,230.0,245.0,250.0),
            "INTC" ->List (19.0,20.0,23.0,24.0,25.0,26.0,27.0,28.0,29.0,30.0,31.0,32.0,33.0,34.0,35.0,36.0,37.0,38.0,39.0,40.0,41.0,42.0,43.0,44.0,45.0,46.0,47.0,48.0,49.0,50.0),
            "JNJ" ->List (70.0,80.0,85.0,87.5,90.0,92.5,95.0,97.5,100.0,105.0,110.0,115.0,120.0,125.0,130.0),
            "JPM" -> List(47.5,50.0,52.5,55.0,57.5,60.0,62.5,65.0,67.5,70.0,72.5,75.0,77.5,80.0,95.0),
            "MCD" -> List(80.0,85.0,87.5,90.0,92.5,95.0,97.5,100.0,105.0,110.0,115.0),
            "MRK" -> List(37.5,40.0,42.5,45.0,47.5,50.0,52.5,55.0,57.5,60.0,62.5,65.0,67.5,70.0,75.0),
            "MSFT" -> List(24.0,25.0,28.0,29.0,30.0,31.0,32.0,33.0,34.0,35.0,36.0,37.0,38.0,39.0,40.0,41.0,42.0,43.0,44.0,45.0,46.0,47.0,48.0,49.0,50.0,52.5,55.0,57.5,60.0),
            "NKE" -> List(70.0,75.0,80.0,85.0,87.5,90.0,92.5,95.0,97.5,100.0,105.0,110.0,115.0,120.0,125.0,130.0,135.0),
            "PFE" -> List(24.0,25.0,26.0,27.0,28.0,29.0,30.0,31.0,32.0,33.0,34.0,35.0,36.0,37.0,38.0,39.0,40.0,41.0,42.0),
            "PG" -> List(60.0,62.5,65.0,67.5,70.0,72.5,75.0,77.5,80.0,82.5,85.0,87.5,90.0,92.5,95.0,100.0,105.0,120.0),
            "TRV" ->List (75.0,80.0,85.0,87.5,90.0,92.5,95.0,97.5,100.0,105.0,110.0,115.0,120.0,125.0),
            "UTX" -> List(90.0,95.0,100.0,105.0,110.0,120.0,125.0,135.0),
            "UNH" -> List(105.0,110.0,115.0,120.0,125.0,130.0,135.0),
            "VZ" ->List(30.0,31.0,32.0,33.0,34.0,35.0,36.0,37.0,38.0,39.0,40.0,41.0,42.0,43.0,44.0,45.0,46.0,47.0,48.0,49.0,50.0,52.5,55.0,57.5,60.0),
            "V" -> List(50.0,60.0,65.0,67.5,70.0,72.5,75.0,77.5,80.0,85.0),
            "WMT" -> List(47.5,55.0,57.5,60.0,62.5,65.0,67.5,70.0,72.5,75.0,77.5,80.0,82.5,85.0)
        )
    
    //Create the system
    val system = ActorSystem("OptionSystem")

    //Calculate # of total strikes (passed to Master so it knows when it's done)
    val strike_list_lengths = strikes.values.map{single_stock => single_stock.size}
    val num_strikes = strike_list_lengths.reduceLeft(_ + _).toInt

    //Create master actor, pass it all the info
    val master = system.actorOf(Props(new Master(num_strikes)))
    master ! new all_stocks_info(prices,sigmas,strikes)

}

class Master(num_strikes: Int) extends Actor{
    //Mutable buffer, will be populated with all the results
    val all_stock_prices: mutable.Buffer[strike_result] = mutable.ArrayBuffer() 

    def receive = {
        //when all stock info is passed, create an actor for each ticker and pass it that stock's info
        case aso:all_stocks_info =>
            val stock_actors = aso.prices.keys.map{ticker =>
                                                    (ticker,context.actorOf(Props(new Stock(aso.strikes(ticker).size))))}.toMap
            stock_actors.keys.foreach{ticker => stock_actors(ticker) ! 
                                    new single_stock_info(ticker,aso.prices(ticker),aso.sigmas(ticker),aso.strikes(ticker))}
                                        
        case sr:strike_result =>
            println(sr.ticker + "," + sr.strike + ": " + sr.value)
            all_stock_prices.append(sr)
            if (all_stock_prices.size >= num_strikes){
                //Create a map of (Ticker,Strike) -> Value
                val price_map = all_stock_prices.map{strike_info =>
                                            ((strike_info.ticker,strike_info.strike),strike_info.value)}.toMap
                context.system.shutdown()
            }
        
    }
}

   


class Stock(num_strikes: Int) extends Actor {
    val single_stock_prices: mutable.Buffer[strike_result] = mutable.ArrayBuffer()
    def receive = {
        case ssi:single_stock_info =>
            val options = ssi.strikes.map{strike => (strike,context.actorOf(Props[Option]))}.toMap
            options.keys.foreach{strike => 
                                    options(strike) ! new single_strike_info(ssi.ticker,ssi.price,ssi.vol,strike)}      
        case sr:strike_result =>
            single_stock_prices.append(sr)
            if (single_stock_prices.size == num_strikes)
                single_stock_prices.foreach{results => context.parent ! results}
    }
}        


class Option extends Actor { 
    def val_period_before(pbp: period_before_params): Double = {
        val prev_size: Int = pbp.values.size - 1
        if (pbp.values.size == 1)
            pbp.values(0)
        else {
            val prev_values = (0 until prev_size).map{i =>
                                                           math.max(pbp.p0 * pbp.values(i) + (1 - pbp.p0) * pbp.values(i+1), 
                                                                    pbp.price * pow(pbp.up,2*i - prev_size) - pbp.strike) }
            val prev_params = period_before_params(prev_values,pbp.price,pbp.strike,pbp.up,pbp.p0)            
            val_period_before(prev_params)
        }
    }
    def americanCall(ci: call_info): Double = {
        val deltaT = ci.time / ci.depth
        val up = exp(ci.sigma * sqrt(deltaT))
        val down = 1 / up
        val p0 = (exp(ci.rf * deltaT) - down) / (up - down)
        val p1 = 1 - p0
        
        val values_time_t = (0 to ci.depth).map{i =>
                                                        math.max(0,ci.price * pow(up,2*i -ci.depth) - ci.strike)}
        
        val pbp = new period_before_params(values_time_t,ci.price,ci.strike,up,p0)
        val_period_before(pbp)
    }
    def receive = {      
        case ssi:single_strike_info => 
            val thiscall = new call_info((34.0 / 256.0),ssi.price,ssi.strike,.0039,ssi.vol,100)
            val result = new strike_result(ssi.ticker,ssi.strike,americanCall(thiscall))
            sender ! result
    }
}


