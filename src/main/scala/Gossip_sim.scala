import scala.util.Random
import akka.actor._
import scala.math._
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala

case class initialize(actornum: Int, gactors: Array[ActorRef],gnum:Int, idneighbor: Array[Int],neighindex:Int)
case class Gossip()
case class Pushsum(ss:Double, ww:Double)

object Gossip_main extends App {
  var start:Long=0 //Timer
  if (args.length == 3) {

    //println("Program started")

    var system = ActorSystem("GossipSimulation")

    var full: String = "full"
    var line: String = "line"
    var threedim: String = "3D"
    var impthreedim: String = "imp3D"

    var gossip: String = "gossip"
    var pushSum: String = "pushsum"

    var numOfNodes: Int = args(0).toInt
    var topology: String = args(1)
    var algorithm: String = args(2)

    //println("arguements assigned")

    //In case of 3D or Improper 3D, nodes changed to 3D matrix with following dimension
    var cr: Double = 0
    cr = math.pow(numOfNodes.toDouble, 1 / 3)
    var dim: Int = cr.toInt + 1
    //println("dim= "+dim)

    var gossipers = Array.ofDim[ActorRef](numOfNodes)
    var i: Int = 0
    while (i < numOfNodes) {
      gossipers(i) = system.actorOf(Props[Gossiper])
      i=i+1;
    }
      //Full
      if (full.equalsIgnoreCase(topology)) {
        var i:Int = 0
        var t:Int = gossipers.length
        while(i<t){
          var neighbors = Array.ofDim[Int] (t)
          var j:Int = 0
          var count:Int=0
          for(j<-0 to t){
            if(j!=i){
              neighbors(count)=j
              count+=1;

            }

          }
          gossipers(i) ! initialize(i, gossipers,t, neighbors,count-1)
          i =i+ 1
        }
    }
    //Line
    if (line.equalsIgnoreCase(topology)) {
      var i: Int = 0
      var t:Int = gossipers.length
      while (i < t) {
        var neighbors = Array.ofDim[Int] (t)
        if (i > 0) neighbors(0)= i - 1
        if (i < t-1) neighbors(1)= i + 1
        gossipers(i) ! initialize(i, gossipers,t,neighbors,2)
        i += 1
      }
    }
    //3D
    if (threedim.equalsIgnoreCase(topology)) {
      //println("Entered 3D")
      var a = Array.ofDim[Int] (dim, dim, dim)
      var neighbors = Array.ofDim[Int] (6)
      var indices = Array.ofDim[Int](numOfNodes)
      for (i <- 0 to numOfNodes - 1) {
        indices(i) = i
      }
      var index: Int = 0;
      var l: Int = 0;
      var k: Int = 0;
      var j: Int = 0;
      var gnode: Int = 0
      for (i <- 0 to dim - 1) {
        for (j <- 0 to dim - 1) {
          for (k <- 0 to dim - 1) {
            a(i)(j)(k) = indices(index);
            index = index + 1;
          }
        }
      }
      for (i <- 0 to dim - 1) {
        for (j <- 0 to dim - 1) {
          for (k <- 0 to dim - 1) {
            }}}


      //var neighindex:Int=0
      for (i <- 0 to dim - 1) {
        for (j <- 0 to dim - 1) {
          for (k <- 0 to dim - 1) {
            l=0

            if (i!=0) //front
            {
              neighbors(l)=a(i-1)(j)(k); l=l+1
               }

            if (i!=dim-1) //back
            {
              neighbors(l)=a(i+1)(j)(k);  l=l+1
            }

            if (j!=0) //left
            {
              neighbors(l)=a(i)(j-1)(k); l=l+1

            }

            if (j!=dim-1) //right
            {
              neighbors(l)=a(i)(j+1)(k); l=l+1

            }

            if (k!=0) //top
            {
              neighbors(l)=a(i)(j)(k-1); l=l+1

            }

            if (k!=dim-1) //bottom
            {
              neighbors(l)=a(i)(j)(k+1); l=l+1
            }

            gnode = a(i)(j)(k)

            gossipers(gnode) ! initialize(gnode, gossipers, numOfNodes,neighbors,l)
          }
        }

      }
      //println("Exiting 3D")
    }
    //Improper 3D
    if (impthreedim.equalsIgnoreCase(topology)) {
      //println("Entered imp3D")
      var a = Array.ofDim[Int] (dim, dim, dim)
      var neighbors = Array.ofDim[Int] (7)
      var indices = Array.ofDim[Int](numOfNodes)
      for (i <- 0 to numOfNodes - 1) {
        indices(i) = i
      }
      var index: Int = 0;
      var l: Int = 0;
      var k: Int = 0;
      var j: Int = 0;
      var r: Int = 0;
      var gnode: Int = 0
      for (i <- 0 to dim - 1) {
        for (j <- 0 to dim - 1) {
          for (k <- 0 to dim - 1) {
            a(i)(j)(k) = indices(index);
            index = index + 1;
          }
        }
      }

      for (i <- 0 to dim - 1) {
        for (j <- 0 to dim - 1) {
          for (k <- 0 to dim - 1) {

          }}}

      var rand: Int =0;

      //var neighindex:Int=0
      for (i <- 0 to dim - 1) {
        for (j <- 0 to dim - 1) {
          for (k <- 0 to dim - 1) {
            l=0

            if (i!=0) //front
            {
              neighbors(l)=a(i-1)(j)(k); l=l+1

            }

            if (i!=dim-1) //back
            {
              neighbors(l)=a(i+1)(j)(k);  l=l+1
            }

            if (j!=0) //left
            {
              neighbors(l)=a(i)(j-1)(k); l=l+1
            }

            if (j!=dim-1) //right
            {
              neighbors(l)=a(i)(j+1)(k); l=l+1

            }

            if (k!=0) //top
            {
              neighbors(l)=a(i)(j)(k-1); l=l+1

            }

            if (k!=dim-1) //bottom
            {

              neighbors(l)=a(i)(j)(k+1); l=l+1

            }
            gnode = a(i)(j)(k)

            rand = Random.nextInt(numOfNodes)
            for (r<-0 to 6)
              if(neighbors(r) == rand){
                rand = Random.nextInt(numOfNodes)
              }
            neighbors(l+1)=rand
            gossipers(gnode) ! initialize(gnode, gossipers, numOfNodes,neighbors,l+1)


          }

        }

      }
      //println("Exiting 3D")
    }
    //Gossip called
    if (algorithm.equalsIgnoreCase(gossip)) {
     // println("Sending for Gossip")
      start=System.currentTimeMillis()
      gossipers(0) ! Gossip
    }
    //Pushsum called
    if (algorithm.equalsIgnoreCase(pushSum)) {
      //println("Sending for PushSum")
      gossipers(0) ! Pushsum(0,0)

    }
    //if(algorithm.equalsIgnoreCase(pushsum))


  }
  else println("Check arguements")

  class Gossiper() extends Actor {
    var end:Long=0
    var time:Long=0
    var actorID: Int = 0
    var tnum:Int=0
    var neighborID = Array.ofDim[Int](tnum)
    var totalnodes = Array.ofDim[ActorRef](tnum)
    var endcount: Int = 0
    var s: Double = actorID.toDouble
    var w: Double = 1.0
    var snew:Double=0.0
    var wnew:Double=0.0
    var ratio:Double=s/w;
    var count:Int=0
    var indexofneighbour:Int=0;
    def receive = {
      //The parameters for each actor are initialized here
      case initialize(actornum: Int, gactors: Array[ActorRef],gnum:Int, idneighbor: Array[Int],neighindex:Int) => {
        actorID = actornum
        neighborID = idneighbor
        tnum=gnum
        totalnodes=gactors;
        indexofneighbour=neighindex
        //println("Initialized" + actorID)
      }

      case Gossip => {
        //println("in gossip")
        endcount = endcount + 1;// checking count for termination(10)
        if (endcount > 10) {
          println("Terminating"+actorID)
          end=System.currentTimeMillis() //Timer End
          time=end-start
          println("Actor "+actorID+" converged in "+time+"milliseconds")
          context.stop(self) // Node killed
        }
        var i: Int = 0
        var c: Int = 0
        for (i <- 0 to tnum) {
          if (i<indexofneighbour) {
            c = c + 1
          }

        }

        var rand: Int = Random.nextInt(c) //Next node called
        println("Sent message from Actor " + actorID + "to " + "Actor "+rand)
        totalnodes(neighborID(rand)) ! Gossip
        if (endcount < 10) (self ! Gossip)
      }
      //Pushsum definition
      case Pushsum(ss:Double, ww:Double) => {
        ratio=s/w

        snew=ss
        wnew=ww

        s=s+snew
        w=w+wnew
        var new_ratio=s/w;
        var comp:Double=math.pow(10,-10)

        if(abs(ratio-new_ratio) < comp) count=count+1 // Termination condition for Pushsum

        if(count>2) {
          println("Terminating"+actorID)
          end=System.currentTimeMillis()
          time=end-start
          println("Actor "+actorID+" converged in "+time+"milliseconds")
          context.stop(self)
        }
        else (self ! Pushsum(s/2,w/2))

        var i: Int = 0
        var c: Int = 0

        for (i <- 0 to tnum) {
          if (i<indexofneighbour) {
            c = c + 1
          }

        }
        println("before rand")
        var rand: Int = Random.nextInt(c)
        println("Sent message from Actor " + actorID + "to " + "Actor "+rand)
        println(neighborID.length)
        totalnodes(neighborID(rand)) ! Pushsum(s/2,w/2) //Next random neighbor node called
        s=s/2
        w=w/2

      }


    }
  }
}
