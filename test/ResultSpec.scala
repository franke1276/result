import java.util.concurrent.TimeUnit

import models._
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


      Result.success(5) mustBe new ResultSuccess(5)

      Result.success(5).map(_ * 5) mustBe new ResultSuccess(25)

      Result.failure[Int]( Problem("p", 500)).map(_ * 5) mustBe new ResultFailure[Int](Problem("p", 500))


      def f(i:Int): Result[String] = if (i == 0) Result.success("-0-") else Result.failure[String](Problem("p", 500))

      Result.success(0).flatMap(f) mustBe Result.success("-0-")
      Result.success(1).flatMap(f) mustBe Result.failure(Problem("p", 500))

      Result.failure(Problem("a", 500)).flatMap(f) mustBe Result.failure(Problem("a", 500))

      Result.fromFuture(Future.successful(Result.success(5))).map(_ * 5).await(t) mustBe Result.success(25)
    }
  }



}
