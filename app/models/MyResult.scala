package models



import play.api.libs.json._
import play.api.mvc.{Result, Results}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}
import scala.util.Try


trait MyResult[+T] {
  def map[A](f: T => A): MyResult[A]

  def flatMap[A](f: T => MyResult[A]): MyResult[A]

  def asFuture: Future[Either[Problem, T]]

  def await(timeout: FiniteDuration): CompletedMyResult[T]

  def asJsonResult(implicit w: Writes[T]): Future[Result]

}

trait CompletedMyResult[+T] extends MyResult[T] {
  def isSuccess: Boolean
  def isFailure: Boolean
}


case class MyResultSuccess[T](value: T) extends CompletedMyResult[T] with Results
{
  def isSuccess: Boolean = true
  def isFailure: Boolean = false

  def map[A](f: T => A): MyResult[A] = MyResultSuccess(f(value))

  def flatMap[A](f: T => MyResult[A]): MyResult[A] = f(value)

  def asFuture: Future[Either[Problem, T]] = Future.successful(Right(value))

  def await(timeout: FiniteDuration): CompletedMyResult[T] = this

  def asJsonResult(implicit w: Writes[T]): Future[Result] = {
    Future.successful(Ok(Json.toJson(value)))
  }
}

case class MyResultFailure[T](problem: Problem) extends CompletedMyResult[T] with Results{
  def isSuccess: Boolean = false
  def isFailure: Boolean = true
  def map[A](f: T => A): MyResult[A] = this.asInstanceOf[MyResult[A]]
  def flatMap[A](f: T => MyResult[A]):MyResult[A] = this.asInstanceOf[MyResult[A]]
  def asFuture: Future[Either[Problem, T]] = Future.successful(Left(problem))

  def await(timeout: FiniteDuration): CompletedMyResult[T] = this.asInstanceOf[CompletedMyResult[T]]

  def asJsonResult(implicit w: Writes[T]): Future[Result] =
    Future.successful(new Status(problem.code)(problem.details))

}

case class MyResultFuture[T](value: Future[MyResult[T]]) extends MyResult[T] {
  implicit val ec = scala.concurrent.ExecutionContext.global

  def asFuture: Future[Either[Problem, T]] = value.flatMap(_.asFuture)

  def map[A](f: T => A): MyResult[A] = MyResultFuture(value.map(_.map(f)))

  def flatMap[A](f: T => MyResult[A]): MyResult[A] = MyResultFuture(value.map(_.flatMap(f)))

  def await(implicit timeout: FiniteDuration): CompletedMyResult[T] = Await.result(value, timeout).await(timeout)

  def asJsonResult(implicit w: Writes[T]): Future[Result] = value.flatMap(_.asJsonResult(w)).recover {
    case t => MyResult.failure(Problem())
  }
}


case class Problem(msg: String, code: Int, details: JsValue = JsNull, cause: Option[Throwable] = None) {
  def withDetails(d: JsValue) = copy(details = d)
  def withDetails(s: String) = copy(details = JsString(s))
  def withCause(t: Throwable) = copy(cause = Some(t))
}


object MyResult {

  def validateJson[T](j: JsValue, p: Problem)(implicit r: Reads[T]): MyResult[T] = {
    j.validate[T] match {
      case JsSuccess(value, _) => success(value)
      case JsError(errors) => failure(p.withDetails(errors.toString()))
    }
  }

  def parseJson(s: String, p: Problem): MyResult[JsValue] = fromTrying(Json.parse(s), p)


  def fromTry[T](f: Try[T], p: Problem): MyResult[T] = {
    f match {
      case scala.util.Success(value) =>  success(value)
      case scala.util.Failure(exception) => failure(p.withCause(exception))
    }
  }

  def fromTrying[T](f: => T, p: Problem): MyResult[T] = fromTry(Try(f), p)


  def fromOption[T](o: Option[T], p: Problem): MyResult[T] = fromEither(o.toRight(p))

  def fromEither[T](e: Either[Problem, T]): MyResult[T] = e match {
    case Right(v) => success(v)
    case Left(e) => failure(e)
  }

  def fromEither[E, T](e: Either[E, T], p: Problem): MyResult[T] = e match {
    case Right(v) => success(v)
    case Left(e) => failure(p.withDetails(e.toString))
  }

  def fromFuture[T](f: Future[MyResult[T]]): MyResult[T] =  MyResultFuture(f)

  def success[T](t: T): MyResult[T] = MyResultSuccess(t)

  def failure[T](e: Problem): MyResult[T] = MyResultFailure[T](e)


}

