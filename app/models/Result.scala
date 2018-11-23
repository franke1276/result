package models



import play.api.http.Writeable
import play.api.libs.json._
import play.api.mvc.{Results, Result => PlayResult}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

object Problem {
  implicit val format = Json.format[Problem]
}

case class Problem(msg: String, code: Int, details: JsArray = Json.arr()) {
  private val httpReturnCodes = new play.api.mvc.Results {}

  def withDetails(d: JsValue) = copy(details = details :+ d)

  def withDetails(s: String) = copy(details = details :+ JsString(s))

  def asJsonResult()(implicit ec: ExecutionContext): Future[PlayResult] = Future.successful(new httpReturnCodes.Status(code)(Json.toJson(this)))
}


object Problems {
  val httpStatus = new play.api.http.Status {}
  val BAD_REQUEST = Problem("BAD_REQUEST", httpStatus.BAD_REQUEST)
  val INTERNAL_SERVER_ERROR = Problem("INTERNAL_SERVER_ERROR", httpStatus.INTERNAL_SERVER_ERROR)
  val NOT_FOUND = Problem("NOT_FOUND", httpStatus.NOT_FOUND)
}


trait Result[+T] {
  def map[A](f: T => A)(implicit ec: ExecutionContext): Result[A]

  def flatMap[A](f: T => Result[A])(implicit ec: ExecutionContext): Result[A]

  def asFuture()(implicit ec: ExecutionContext): Future[Either[Problem, T]]

  def await(timeout: FiniteDuration): CompletedResult[T] = this.asInstanceOf[CompletedResult[T]]

  def asJsonPlayResult()(implicit w: Writes[T], ec: ExecutionContext): Future[PlayResult]

  def asPlayResult()(implicit c: Writeable[T], ec: ExecutionContext): Future[PlayResult]

}

trait CompletedResult[+T] extends Result[T] {
  def isSuccess: Boolean

  def isFailure: Boolean
}


case class ResultSuccess[T](value: T) extends CompletedResult[T] with Results {
  def isSuccess: Boolean = true

  def isFailure: Boolean = false

  def map[A](f: T => A)(implicit ec: ExecutionContext): Result[A] = ResultSuccess(f(value))

  def flatMap[A](f: T => Result[A])(implicit ec: ExecutionContext): Result[A] = f(value)

  def asFuture()(implicit ec: ExecutionContext): Future[Either[Problem, T]] = Future.successful(Right(value))

  def asJsonPlayResult()(implicit w: Writes[T], ec: ExecutionContext): Future[PlayResult] = map(v => Json.toJson(v)).asPlayResult()

  def asPlayResult()(implicit c: Writeable[T], ec: ExecutionContext): Future[PlayResult] = Future.successful(Ok(value))

}

case class ResultFailure[T](problem: Problem) extends CompletedResult[T] with Results {
  def isSuccess: Boolean = false

  def isFailure: Boolean = true

  def map[A](f: T => A)(implicit ec: ExecutionContext): Result[A] = this.asInstanceOf[Result[A]]

  def flatMap[A](f: T => Result[A])(implicit ec: ExecutionContext): Result[A] = this.asInstanceOf[Result[A]]

  def asFuture()(implicit ec: ExecutionContext): Future[Either[Problem, T]] = Future.successful(Left(problem))

  def asJsonPlayResult()(implicit w: Writes[T], ec: ExecutionContext): Future[PlayResult] = problem.asJsonResult()

  def asPlayResult()(implicit c: Writeable[T], ec: ExecutionContext): Future[PlayResult] = Future.successful(new Status(problem.code)(problem.toString))

}

case class FutureResult[T](value: Future[Result[T]]) extends Result[T] {

  import Result._

  def asFuture()(implicit ec: ExecutionContext): Future[Either[Problem, T]] = value.flatMap(_.asFuture)

  def map[A](f: T => A)(implicit ec: ExecutionContext): Result[A] = FutureResult(value.map(_.map(f)))

  def flatMap[A](f: T => Result[A])(implicit ec: ExecutionContext): Result[A] = FutureResult(value.map(_.flatMap(f)))

  def asJsonPlayResult()(implicit w: Writes[T], ec: ExecutionContext): Future[PlayResult] = value.flatMap(_.asJsonPlayResult()).recoverWith {
    case t => failure(Problem("Internal Server Error", 500, Json.arr(JsString(s"${t.getClass.getName}: ${t.getMessage}")))).asJsonPlayResult
  }

  def asPlayResult()(implicit c: Writeable[T], ec: ExecutionContext): Future[PlayResult] = value.flatMap(_.asPlayResult()).recoverWith {
    case t => failure(Problem("Internal Server Error", 500, Json.arr(JsString(s"${t.getClass.getName}: ${t.getMessage}")))).asPlayResult
  }

  override def await(timeout: FiniteDuration): CompletedResult[T] = Await.result(value, timeout).await(timeout)
}


object Result {

  def validateJson[T](j: JsValue, p: Problem)(implicit r: Reads[T]): Result[T] = {
    j.validate[T] match {
      case JsSuccess(value, _) => success(value)
      case JsError(errors) => failure(p.withDetails(errors.toString()))
    }
  }

  def parseJson(s: String, p: Problem): Result[JsValue] = fromTrying(Json.parse(s), p)


  def fromTry[T](f: Try[T], p: Problem): Result[T] = {
    f match {
      case scala.util.Success(value) => success(value)
      case scala.util.Failure(t) => failure(p.withDetails(JsString(s"${t.getClass.getName}: ${t.getMessage}")))
    }
  }

  def fromTrying[T](f: => T, p: Problem): Result[T] = fromTry(Try(f), p)

  def fromOption[T](o: Option[T], p: Problem): Result[T] = fromEither(o.toRight(p))

  def fromCondition(b: Boolean, p: Problem): Result[Unit] = fromOption(if (b) Some(()) else None, p)

  def fromEither[T](e: Either[Problem, T]): Result[T] = e match {
    case Right(v) => success(v)
    case Left(e) => failure(e)
  }

  def fromEither[E, T](e: Either[E, T], p: Problem): Result[T] = e match {
    case Right(v) => success(v)
    case Left(e) => failure(p.withDetails(e.toString))
  }

  def fromFuture[T](f: Future[Result[T]]): Result[T] = FutureResult(f)

  def fromFutureSuccess[T](f: Future[T])(implicit ec: ExecutionContext): Result[T] = FutureResult(f.map(success))

  def success[T](t: T): Result[T] = ResultSuccess(t)

  def failure[T](e: Problem): Result[T] = ResultFailure[T](e)


}

