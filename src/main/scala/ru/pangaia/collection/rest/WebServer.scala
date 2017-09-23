package ru.pangaia.collection.rest

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.Done
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import collection.entity._
import spray.json.DefaultJsonProtocol._
import spray.json.{DefaultJsonProtocol, JsArray, JsObject, JsString, JsValue, RootJsonFormat}

import scala.io.StdIn
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object WebServer {

  // formats for unmarshalling and marshalling

  implicit object fieldFormat extends RootJsonFormat[CardField]
  {
    override def read(json: JsValue): CardField = ???

    override def write(obj: CardField): JsValue = obj match
    {
      case IntField(name, description) => JsObject(Map(
        "name" -> JsString(name),
        "description" -> JsString(description)))
      case StringField(name, description) => JsObject(Map(
        "name" -> JsString(name),
        "description" -> JsString(description)))
      case ChoiceField(name, description, possibleChoices) => JsObject(Map(
        "name" -> JsString(name),
        "description" -> JsString(description),
        "possibleValues" -> JsArray(possibleChoices.map((s:String) => JsString(s)).toVector)))
    }
  }
  implicit val recordFormat: RootJsonFormat[Record] = jsonFormat2(Record)
  implicit val collectibleFormat: RootJsonFormat[Collectible] = jsonFormat3(Collectible)
  implicit val cardFormat: RootJsonFormat[CatalogCard] = jsonFormat2(CatalogCard)


  // (fake) async database query api
  def fetchItem: Future[Option[CatalogCard]] = Future {Some(ACard.card)}
  //  def saveOrder(order: Order): Future[Done] = ???

  def main(args: Array[String]) = {

    // needed to run the route
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    // needed for the future map/flatmap in the end
    implicit val executionContext = system.dispatcher

    val route: Route =
      get {
        val maybeItem: Future[Option[CatalogCard]] = fetchItem

        onSuccess(maybeItem) {
          case Some(item) => complete(item)
          case None       => complete(StatusCodes.NotFound)
        }

      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ ⇒ system.terminate()) // and shutdown when done
  }
}