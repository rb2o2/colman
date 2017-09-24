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
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object WebServer {

  // formats for unmarshalling and marshalling
  implicit val categoryFormat: RootJsonFormat[Cat] = new RootJsonFormat[Cat]
  {
    override def write(obj: Cat): JsValue = JsObject(Map(
        "id" -> JsString(obj.id.toString),
        "createdOn" -> JsString(obj.createdOn.getTime.toString),
        "name" -> JsString(obj.name),
        "description" -> JsString(obj.description)))

    override def read(json: JsValue): Cat = ???
  }

  implicit val treeFormat: RootJsonFormat[CategoryNode] = new RootJsonFormat[CategoryNode]
  {
    override def write(obj: CategoryNode): JsValue =

      JsObject(Map(
        "index" -> JsString(obj.index),
        "value" -> JsObject(Map(
          "name" -> JsString(obj.value.name),
          "description" -> JsString(obj.value.description))),
        "children" ->
          {
            if (obj.children.isEmpty) JsArray()
            else JsArray(obj.children.map(write).toVector)
          }))

    override def read(json: JsValue): CategoryNode = ???
  }

  implicit object fieldFormat extends RootJsonFormat[CardField]
  {
    override def read(json: JsValue): CardField = ???

    override def write(obj: CardField): JsValue = obj match
    {
      case i:IntField=> JsObject(Map(
        "name" -> JsString(i.name),
        "description" -> JsString(i.description),
        "id" -> JsString(i.id.toString),
        "createdOn" -> JsString(i.createdOn.getTime.toString)))
      case i:StringField => JsObject(Map(
        "name" -> JsString(i.name),
        "description" -> JsString(i.description),
        "id" -> JsString(i.id.toString),
        "createdOn" -> JsString(i.createdOn.getTime.toString)))
      case i:ChoiceField => JsObject(Map(
        "name" -> JsString(i.name),
        "description" -> JsString(i.description),
        "possibleValues" -> JsArray(i.possibleChoices.map((s:String) => JsString(s)).toVector),
        "id" -> JsString(i.id.toString),
        "createdOn" -> JsString(i.createdOn.getTime.toString)))
      case t:TaxonField => JsObject(Map(
        "name" -> JsString(t.name),
        "description" -> JsString(t.description),
        "root" -> treeFormat.write(t.root),
        "id" -> JsString(t.id.toString),
        "createdOn" -> JsString(t.createdOn.getTime.toString)))
      case _ => JsArray()
    }
  }
  implicit val recordFormat: RootJsonFormat[Record] = new RootJsonFormat[Record]
  {
    override def write(obj: Record): JsValue = JsObject(Map(
      "id" -> JsString(obj.id.toString),
      "createdOn" -> JsString(obj.createdOn.getTime.toString),
      "field" -> fieldFormat.write(obj.field),
      "value" -> JsString(obj.value)))

    override def read(json: JsValue): Record = ???
  }
  implicit val collectibleFormat: RootJsonFormat[Collectible] = new RootJsonFormat[Collectible]
  {
    override def write(obj: Collectible): JsValue = JsObject(Map(
      "id" -> JsString(obj.id.toString),
      "createdOn" -> JsString(obj.createdOn.getTime.toString),
      "fields" -> JsArray(obj.fields.map(fieldFormat.write).toVector),
      "name" -> JsString(obj.name),
      "description" -> JsString(obj.description)))

    override def read(json: JsValue): Collectible = ???
  }
  implicit val cardFormat: RootJsonFormat[CatalogCard] = new RootJsonFormat[CatalogCard]
  {
    override def write(obj: CatalogCard): JsValue = JsObject(Map(
      "id" -> JsString(obj.id.toString),
      "createdOn" -> JsString(obj.createdOn.getTime.toString),
      "records" -> JsArray(obj.records.map(recordFormat.write).toVector),
      "coll" -> collectibleFormat.write(obj.coll)))

    override def read(json: JsValue): CatalogCard = ???
  }


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

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ ⇒ system.terminate()) // and shutdown when done
  }
}