package ru.pangaia.collection.rest

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import ru.pangaia.collection.entity._

import scala.io.StdIn
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object WebServer {

  import ru.pangaia.collection.entity.Marshallers._

  // (fake) async database query api
  def fetchItem: Future[Option[CatalogCard]] = Future {Some(ACard.card)}
  //  def saveOrder(order: Order): Future[Done] = ???

  def main(args: Array[String]): Unit = {

    // needed to run the route
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    // needed for the future map/flatmap in the end
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val route: Route =
      get {
        val maybeItem: Future[Option[CatalogCard]] = fetchItem

        onSuccess(maybeItem) {
          case Some(item) => complete(item)
          case None       => complete(StatusCodes.NotFound)
        }
      }
    val host = "localhost"
    val port = 8080
    val bindingFuture = Http().bindAndHandle(route, host, port)
    println(s"Server online at http://$host:$port/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ ⇒ system.terminate()) // and shutdown when done
  }
}