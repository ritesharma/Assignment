package scala
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global



  object ListHandling extends App {

    val powersOfTwoClean = List[Int](8, 32, 2, 16384, 512)
    val powersOfTwo = List[Int](8, 32, 2, 3, 16384, 512, 9)
    // if (x + 1) of above lists is a multiple of 3

    val (multipleOfThree, notMultipleOfThree) = multipleOfThreeAndNotMultipleOfThree(powersOfTwo,powersOfTwoClean)

    println(s"${multipleOfThree}, ${notMultipleOfThree}")
    // example - 8  is 2^3 so the power value is 3. If a number is not a power of 2 print a message and discard it
    val powerValues = getPowerOfTwoValues(powersOfTwo)

    println(s" ${powerValues}")
    val ascendingSortedPowerValues = sortTheList(powerValues)
    println(s"${ascendingSortedPowerValues}")
    // take 2nd element to n-2nd element and sum the values and print it
    val slicedListSum : Int = sliceListSum(ascendingSortedPowerValues)
    println(s"${slicedListSum}")

    def multipleOfThreeAndNotMultipleOfThree (powerOfTwo : List[Int], powerOfTwoClean : List[Int]): (List[Int],List[Int]) ={

      val multipleOfThree : List[Int] = (powerOfTwo ++ powerOfTwoClean).filter{
        x => (x+1)%3 == 0
      }
      val notMultipleOfThree : List[Int]= (powerOfTwo ++ powerOfTwoClean).filter{
        x => (x+1)%3 != 0
      }

      (multipleOfThree,notMultipleOfThree)
    }

    def getPowerOfTwoValues(powersOfTwo : List[Int]) : List[Int]={

      //      val powerOfTwo : List[Int]=powersOfTwo.filter{
      //
      //        x => if((x&(x-1))==0){
      //          true
      //        } else {
      //          println(s" ${x} is not power of 2 ")
      //          false
      //        }
      //
      //      }
      //      powerOfTwo



      //      val valueOfPower : List[Int] =powersOfTwo.filter{ x =>
      //        if((x&(x-1))==0){
      //          true
      //        } else {
      //          println(s" ${x} is not power of 2 ")
      //          false
      //        }
      //      }.map{ x =>
      //        var count : Int = 0;
      //        var valueOfElement : Int=x
      //        while(valueOfElement>1){
      //          count+=1
      //          valueOfElement /=2
      //        }
      //        count
      //      }
      //      valueOfPower


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

    def sortTheList( powerValues : List[Int]) : List[Int] = {
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
    val jobsEnrichedWithStats: Future[List[Job]] = for{
      job <- jobs
      click <- clicks
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
    }// It should contain => Future(List(Job("job1", "title1"), Job("job2", "title1", Some(50), None), Job("job3", "title3", None, Some(150))))


    jobsEnrichedWithStats.foreach{
      x: List[Job] => println(s"${x}")
    }

    Thread.sleep(5000)
    val jobsWithClicksNotNone = ???
    val jobsWithAppliesNotNone = ???
    //  title -> List[Job]
    val jobsGroupedByTitle = ???
    // Should return Map(title -> (sumClicks, sumApplies)). if clicks/applies is None, set its value as 0
    val statsPerTitle = ???



  }


