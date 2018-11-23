package controllers

import models.{Problems, Result}
import play.api.i18n.Langs
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import services.{GreetingService, Message}

import scala.concurrent.ExecutionContext.Implicits._

class GreeterController(greetingService: GreetingService,
                        langs: Langs,
                        cc: ControllerComponents) extends AbstractController(cc) {



  def index(lang: String): Action[AnyContent] = Action.async {


//   greetingService.greetingMessage(lang).map(m => views.html.index(m)).asPlayResult()
   greetingService.greetingMessage(lang).asJsonPlayResult()

  }

  def data() = Action.async(parse.tolerantText) { req =>
    (for {
      j <- Result.parseJson(req.body, Problems.BAD_REQUEST.withDetails("no json"))
      m <- Result.validateJson[Message](j, Problems.BAD_REQUEST.withDetails("not a message"))
    } yield m).asJsonPlayResult()
  }

}
