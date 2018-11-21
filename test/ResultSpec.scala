import java.util.concurrent.TimeUnit

import models.{MyResult, MyResultFailure, MyResultSuccess, SimpleProblem}
import org.scalatestplus.play.{BaseOneAppPerTest, PlaySpec}
import play.api.test.Helpers._
import play.api.test._

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

/**
 *
 */
class ResultSpec extends PlaySpec {

  "MyResult" should {
    "run" in {

      implicit val t = FiniteDuration(10,TimeUnit.SECONDS)


      MyResult.success(5) mustBe new MyResultSuccess(5)

      MyResult.success(5).map(_ * 5) mustBe new MyResultSuccess(25)

      MyResult.failure[Int]( SimpleProblem("p")).map(_ * 5) mustBe new MyResultFailure[Int](SimpleProblem("p"))


      def f(i:Int): MyResult[String] = if (i == 0) MyResult.success("-0-") else MyResult.failure[String](SimpleProblem("p"))

      MyResult.success(0).flatMap(f) mustBe MyResult.success("-0-")
      MyResult.success(1).flatMap(f) mustBe MyResult.failure(SimpleProblem("p"))

      MyResult.failure(SimpleProblem("a")).flatMap(f) mustBe MyResult.failure(SimpleProblem("a"))

      MyResult.fromFuture(Future.successful(MyResult.success(5))).map(_ * 5).await(t) mustBe MyResult.success(25)
    }
  }



}
