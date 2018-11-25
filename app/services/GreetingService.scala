package services

import models.{Problem, Problems, Result}
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext
import scalaz.zio._

case class Message(value: String)

object Message {
  implicit val format = Json.format[Message]
}

class GreetingService(dao: MessageDao) {

  def greetingMessage(language: String)(implicit ec: ExecutionContext): Result[Message] =
    for {
      _ <- Result.fromCondition(language.toLowerCase == "de", Problems.BAD_REQUEST.withDetails("Language is not DE"))
      l <- dao.loadMessage(language)
    } yield l.copy(value = s"${l.value} jippi")


  def greetingMessageIO(language: String)(implicit ec: ExecutionContext): IO[Problem, Message] =
    for {
      _<- fromCondition(language.toLowerCase == "de", Problems.BAD_REQUEST.withDetails("Language is not DE"))
      l <- dao.loadMessageIO(language)

    } yield l.copy(value = s"${l.value} jippi")


  def fromCondition(b: Boolean, p: Problem): IO[Problem, Boolean] = {
    if (b) IO.point(b) else IO.fail(p)
  }
}
