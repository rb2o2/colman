package ru.pangaia.collection.entity

import java.sql.Timestamp

import spray.json.{JsArray, JsObject, JsString, JsValue, RootJsonFormat}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._

object Marshallers
{
  // formats for unmarshalling and marshalling
  implicit val categoryFormat: RootJsonFormat[Cat] = new RootJsonFormat[Cat]
  {
    override def write(obj: Cat): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
      "createdOn" -> LongJsonFormat.write(obj.createdOn.getTime),
      "modifiedOn" -> (obj.modifiedOn match
      {
        case Some(s: Timestamp) => LongJsonFormat.write(s.getTime)
        case None => LongJsonFormat.write(0)
      }),
      "name" -> JsString(obj.name),
      "description" -> JsString(obj.description)))

    override def read(json: JsValue): Cat = ???
  }

  implicit val collectionFormat: RootJsonFormat[Collection] = new RootJsonFormat[Collection]
  {
    override def write(obj: Collection): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
      "createdOn" -> LongJsonFormat.write(obj.createdOn.getTime),
      "modifiedOn" -> (obj.modifiedOn match
      {
        case Some(s: Timestamp) => LongJsonFormat.write(s.getTime)
        case None => LongJsonFormat.write(0)
      }),
      "name" -> JsString(obj.name),
      "description" -> JsString(obj.description),
      "cards" -> JsArray(obj.list.map(card => cardFormat.write(card)).toVector),
      "collectible" -> collectibleFormat.write(obj.coll)
    ))

    override def read(json: JsValue): Collection = ???
  }

  implicit val treeFormat: RootJsonFormat[CategoryNode] = new RootJsonFormat[CategoryNode]
  {
    override def write(obj: CategoryNode): JsValue =

      JsObject(Map(
        "index" -> JsString(obj.index),
        "value" -> categoryFormat.write(obj.value),
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
      case i: IntField=> JsObject(Map(
        "name" -> JsString(i.name),
        "description" -> JsString(i.description),
        "id" -> LongJsonFormat.write(i.id)
//        ,"createdOn" -> LongJsonFormat.write(i.createdOn.getTime)
       ))
      case i: StringField => JsObject(Map(
        "name" -> JsString(i.name),
        "modifiedOn" -> (obj.modifiedOn match
        {
          case Some(s: Timestamp) => LongJsonFormat.write(s.getTime)
          case None => LongJsonFormat.write(0)
        }),
        "description" -> JsString(i.description),
        "id" -> LongJsonFormat.write(i.id)
//        ,"createdOn" -> LongJsonFormat.write(i.createdOn.getTime)
       ))
      case i: ChoiceField => JsObject(Map(
        "name" -> JsString(i.name),
        "description" -> JsString(i.description),
        "possibleValues" -> JsArray(i.possibleChoices.map((s:String) => JsString(s)).toVector),
        "id" -> LongJsonFormat.write(i.id)
//        ,"createdOn" -> LongJsonFormat.write(i.createdOn.getTime)
       ))
      case t: TaxonField => JsObject(Map(
        "name" -> JsString(t.name),
        "description" -> JsString(t.description),
        "root" -> treeFormat.write(t.root),
        "id" -> LongJsonFormat.write(t.id)
//        ,"createdOn" -> LongJsonFormat.write(t.createdOn.getTime)
       ))
      case c: BooleanField => JsObject(Map(
        "name" -> JsString(c.name),
        "description" -> JsString(c.description),
        "id" -> LongJsonFormat.write(c.id)
//        ,"createdOn" -> LongJsonFormat.write(c.createdOn.getTime)
      ))
      case _ => JsArray()
    }
  }
  implicit val recordFormat: RootJsonFormat[Record] = new RootJsonFormat[Record]
  {
    override def write(obj: Record): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
//      "modifiedOn" -> (obj.modifiedOn match
//      {
//        case Some(s: Timestamp) => LongJsonFormat.write(s.getTime)
//        case None => LongJsonFormat.write(0)
//      }),
//      "createdOn" -> LongJsonFormat.write(obj.createdOn.getTime),
//      "field" -> fieldFormat.write(obj.field),
      "fieldName" -> JsString(obj.field.name),
      "value" -> JsString(obj.value)))

    override def read(json: JsValue): Record = ???
  }
  implicit val collectibleFormat: RootJsonFormat[Collectible] = new RootJsonFormat[Collectible]
  {
    override def write(obj: Collectible): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
//      "createdOn" -> LongJsonFormat.write(obj.createdOn.getTime),
      "fields" -> JsArray(obj.fieldsVector.map(fieldFormat.write)),
      "name" -> JsString(obj.name),
      "description" -> JsString(obj.description)))

    override def read(json: JsValue): Collectible = ???
  }
  implicit val cardFormat: RootJsonFormat[CatalogCard] = new RootJsonFormat[CatalogCard]
  {
    override def write(obj: CatalogCard): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
//      "createdOn" -> LongJsonFormat.write(obj.createdOn.getTime),
      "records" -> JsArray(obj.records.map(recordFormat.write)),
      "collectibleName" -> JsString(obj.coll.name)))
//      "coll" -> collectibleFormat.write(obj.coll)))

    override def read(json: JsValue): CatalogCard = ???
  }
}
