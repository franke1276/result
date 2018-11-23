package services

import akka.actor.ActorSystem
import models.{Result, Problems}

import scala.concurrent.{ExecutionContext, Promise}
import scala.concurrent.duration._

class MessageDao(sys: ActorSystem) {

  def loadMessage(key: String)(implicit ec: ExecutionContext): Result[Message] = {
    val p = Promise[Message]()

    sys.scheduler.scheduleOnce(500.milliseconds)(
      if (System.currentTimeMillis() % 2 == 0)
        p.success(Message(s"key: $key"))
      else
        p.failure(new RuntimeException("db error"))
    )(sys.dispatcher)

    for {
      _ <- Result.fromCondition(key == "DE", Problems.NOT_FOUND.withDetails(s"key $key not found"))
      r <- Result.fromFutureSuccess(p.future)
    } yield r

  }

}
