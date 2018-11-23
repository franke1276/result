package services

import models.{Result, Problems}
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext

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

}
