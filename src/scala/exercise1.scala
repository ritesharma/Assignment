package scala
import scala.FuturesOptionsHandling.clicks
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration



  object ListHandling extends App {

    val powersOfTwoClean = List[Int](8, 32, 2, 16384, 512)
    val powersOfTwo = List[Int](8, 32, 2, 3, 16384, 512, 9)
    // if (x + 1) of above lists is a multiple of 3

    val (multipleOfThree, notMultipleOfThree) = (getListOfMultiplesOfThree() , getListOfNotMultiplesOfThree())
    println(s"${multipleOfThree}, ${notMultipleOfThree}")

    // example - 8  is 2^3 so the power value is 3. If a number is not a power of 2 print a message and discard it
    val powerValues = getPowerOfTwoValues()
    println(s" ${powerValues}")

    val ascendingSortedPowerValues = sortTheList()
    println(s"${ascendingSortedPowerValues}")

    // take 2nd element to n-2nd element and sum the values and print it
    val slicedListSum : Int = sliceListSum(ascendingSortedPowerValues)
    println(s"${slicedListSum}")

    def getListOfMultiplesOfThree ()={
        (powersOfTwo ++ powersOfTwoClean).filter{ x =>
        (x+1)%3 == 0
      }
    }

    def getListOfNotMultiplesOfThree () ={
      (powersOfTwo ++ powersOfTwoClean).filter{ x =>
        (x+1)%3 != 0
      }
    }

    def getPowerOfTwoValues() : List[Int]={
      powersOfTwo.filter{ x =>
        if((x&(x-1))==0){
          true
        } else {
          println(s" ${x} is not power of 2 ")
          false
        }
      }.map{ x =>
        var count : Int = 0;
        var valueOfElement : Int=x
        while(valueOfElement>1){
          count+=1
          valueOfElement /=2
        }
        count
      }
    }

    def sortTheList() : List[Int] = {
      powerValues.sorted
    }

    def sliceListSum( list : List[Int]) : Int = {
      list.drop(2).dropRight(2).sum;
    }


  }



  //Hint - Try map/for-comprehension on Futures.
  //Use .copy() to make copies of immutable objects in which you want to update data
  object FuturesOptionsHandling extends App {
    case class Job(jobId: String, title: String, clicks: Option[Int] = None, applies: Option[Int] = None)
    case class ClicksStat(jobId: String, clicks: Int)
    case class AppliesStat(jobId: String, applies: Int)
    val jobs: Future[List[Job]] = Future.successful(List(Job("job1", "title1"), Job("job2", "title1"), Job("job3", "title3")))
    val clicks = Future.successful(List(ClicksStat("job2", 50),ClicksStat("job3", 20)))
    val applies = List(AppliesStat("job3", 150))

    // If stats are not present clicks/Applies should be None
    val jobsEnrichedWithStats = jobsEnrichedWithStat()// It should contain => Future(List(Job("job1", "title1"), Job("job2", "title1", Some(50), None), Job("job3", "title3", None, Some(150))))
    Await.result(jobsEnrichedWithStats , Duration.Inf)
    jobsEnrichedWithStats.foreach{
      x => println(s"${x}")
    }


    val jobsWithClicksNotNone =  jobsWithClickNotNone()
    Await.result(jobsWithClicksNotNone, Duration.Inf)
    jobsWithClicksNotNone.foreach{
      x: List[Job] => println(s"${x}")
    }

    val jobsWithAppliesNotNone = jobsWithApplieNotNone()
    Await.result(jobsWithAppliesNotNone, Duration.Inf)
    jobsWithAppliesNotNone.foreach{
      x => println(s"${x}")
    }

    //  title -> List[Job]
    val jobsGroupedByTitle =  getListOfJobsGroupedByTitle()
    Await.result(jobsGroupedByTitle , Duration.Inf)

    jobsGroupedByTitle.foreach{ x=>
      println(x)
    }

    // Should return Map(title -> (sumClicks, sumApplies)). if clicks/applies is None, set its value as 0
    val statsPerTitle =  getSumOfClicksAndAplliesByTitle()
    Await.result( statsPerTitle , Duration.Inf)

    statsPerTitle.foreach{
      x =>
        println(x)
    }


    def jobsEnrichedWithStat() : Future[List[Job]] ={
      for{
        job <- this.jobs
        click <- this.clicks
      } yield{
        val updatedJobListWithClick= job.map{
          itr =>
            (click.find(_.jobId==itr.jobId) , applies.find(_.jobId==itr.jobId)) match {
              case (Some(v), Some(v1))=> itr.copy(clicks=Some{v.clicks}, applies=Some{v1.applies})
              case (None , Some(v1))=> itr.copy(applies=Some{v1.applies})
              case (Some(v) , None )=> itr.copy(clicks=Some{v.clicks})
              case (None  , None )=> itr
            }
        }
        updatedJobListWithClick
      }
    }

    def jobsWithClickNotNone (): Future[List[Job]] ={
      jobsEnrichedWithStats.map{ listOfJob =>

        listOfJob.filter{ job =>
          if(job.clicks==None) false else true
        }
      }
    }

    def jobsWithApplieNotNone (): Future[List[Job]] ={
      jobsEnrichedWithStats.map{ listOfJob =>
        listOfJob.filter{ job =>
          if(job.applies==None) false else true;
        }
      }
    }

    def getListOfJobsGroupedByTitle(): Future[Map[String, List[Job]]] ={
      jobsEnrichedWithStats.map{ listOfJob =>
        listOfJob.groupBy(_.title)
      }
    }

    def getSumOfClicksAndAplliesByTitle(): Future[Map[String, (Int, Int)]] ={
      jobsGroupedByTitle.map{ mapOfJobByTitle =>

        mapOfJobByTitle.map{ case (key ,value) =>
            val sumOfClick = value.map{
              x => x.clicks.getOrElse(0)
            } .sum
            val sumOfApplies = value.map{
              x => x.applies.getOrElse(0)
            } .sum

            (key -> (sumOfClick,sumOfApplies))
        }
      }
    }


  }


