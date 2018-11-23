package services

import akka.actor.ActorSystem

trait ServicesModule {

  import com.softwaremill.macwire._
  def actorSystem: ActorSystem

  lazy val messageDao = wire[MessageDao]
  lazy val greetingService = wire[GreetingService]

}
