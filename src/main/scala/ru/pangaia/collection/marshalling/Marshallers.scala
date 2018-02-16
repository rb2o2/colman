package ru.pangaia.collection.marshalling

import ru.pangaia.collection.model._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.collection.mutable

object Marshallers
{
  // formats for unmarshalling and marshalling
  implicit val categoryFormat: RootJsonFormat[Cat] = new RootJsonFormat[Cat]
  {
    override def write(obj: Cat): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
      "index" -> JsString(obj.index),
      "name" -> JsString(obj.name),
      "description" -> JsString(obj.description)))

    override def read(json: JsValue): Cat = Cat(
      StringJsonFormat.read(json.asJsObject.fields("index")),
      StringJsonFormat.read(json.asJsObject.fields("name")),
      StringJsonFormat.read(json.asJsObject.fields("description"))
    )
  }

  implicit val collectionFormat: RootJsonFormat[Collection] = new RootJsonFormat[Collection]
  {
    override def write(obj: Collection): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
      "name" -> JsString(obj.name),
      "description" -> JsString(obj.description),
      "cards" -> JsArray(obj.list.map(card => cardFormat.write(card)).toVector),
      "collectible" -> collectibleFormat.write(obj.coll)
    ))

    override def read(json: JsValue): Collection = {
      val c = Collection(
        collectibleFormat.read(json.asJsObject.fields("collectible")),
        StringJsonFormat.read(json.asJsObject.fields("name")),
        StringJsonFormat.read(json.asJsObject.fields("description"))
      )
      RootJsArrayFormat.read(json.asJsObject.fields("cards")).elements.map(cardFormat.read).foreach(c.add)
      c
    }
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

    override def read(json: JsValue): CategoryNode = CategoryNode(
      StringJsonFormat.read(json.asJsObject.fields("index")),
      categoryFormat.read(json.asJsObject.fields("value")),
      RootJsArrayFormat.read(json.asJsObject.fields("children")).elements.map(read).toBuffer
    )
  }

  implicit object fieldFormat extends RootJsonFormat[CardField]
  {
    override def read(json: JsValue): CardField = {
      val j = StringJsonFormat.read(json.asJsObject.fields("type"))
      j match {
        case "IntField" => IntField(
          StringJsonFormat.read(json.asJsObject.fields("name")),
          StringJsonFormat.read(json.asJsObject.fields("description"))
        )
        case "StringField" => StringField(
          StringJsonFormat.read(json.asJsObject.fields("name")),
          StringJsonFormat.read(json.asJsObject.fields("description"))
        )
        case "BooleanField" => BooleanField(
          StringJsonFormat.read(json.asJsObject.fields("name")),
          StringJsonFormat.read(json.asJsObject.fields("description"))
        )
        case "ChoiceField" => ChoiceField(
          StringJsonFormat.read(json.asJsObject.fields("name")),
          StringJsonFormat.read(json.asJsObject.fields("description")),
          RootJsArrayFormat.read(json.asJsObject.fields("possibleValues")).elements
            .map(StringJsonFormat.read).toSet
        )
        case "TaxonField" => TaxonField(
          StringJsonFormat.read(json.asJsObject.fields("name")),
          StringJsonFormat.read(json.asJsObject.fields("description")),
          treeFormat.read(json.asJsObject.fields("root"))
        )
        case _ => throw new IllegalArgumentException
      }
    }

    override def write(obj: CardField): JsValue = obj match
    {
      case i: IntField => JsObject(Map(
        "type" -> JsString("IntField"),
        "name" -> JsString(i.name),
        "description" -> JsString(i.description),
        "id" -> LongJsonFormat.write(i.id)
       ))
      case i: StringField => JsObject(Map(
        "type" -> JsString("StringField"),
        "name" -> JsString(i.name),
        "description" -> JsString(i.description),
        "id" -> LongJsonFormat.write(i.id)
       ))
      case i: ChoiceField => JsObject(Map(
        "type" -> JsString("ChoiceField"),
        "name" -> JsString(i.name),
        "description" -> JsString(i.description),
        "possibleValues" -> JsArray(i.possibleChoices.map((s:String) => JsString(s)).toVector),
        "id" -> LongJsonFormat.write(i.id)
       ))
      case t: TaxonField => JsObject(Map(
        "type" -> JsString("TaxonField"),
        "name" -> JsString(t.name),
        "description" -> JsString(t.description),
        "root" -> treeFormat.write(t.root),
        "id" -> LongJsonFormat.write(t.id)
       ))
      case c: BooleanField => JsObject(Map(
        "type" -> JsString("BooleanField"),
        "name" -> JsString(c.name),
        "description" -> JsString(c.description),
        "id" -> LongJsonFormat.write(c.id)
      ))
      case _ => JsArray()
    }
  }
  implicit val recordFormat: RootJsonFormat[Record] = new RootJsonFormat[Record]
  {
    override def write(obj: Record): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
      "fieldName" -> JsString(obj.field.name),
      "value" -> JsString(obj.value)))

    override def read(json: JsValue): Record = ??? //TODO Record.Field?
  }
  implicit val collectibleFormat: RootJsonFormat[Collectible] = new RootJsonFormat[Collectible]
  {
    override def write(obj: Collectible): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
      "fields" -> JsArray(obj.fieldsVector.map(fieldFormat.write)),
      "name" -> JsString(obj.name),
      "description" -> JsString(obj.description)))

    override def read(json: JsValue): Collectible = {
      Collectible(
        StringJsonFormat.read(json.asJsObject.fields("name")),
        StringJsonFormat.read(json.asJsObject.fields("description")),
        mutable.Map(
          RootJsArrayFormat.read(json.asJsObject.fields("fields")).elements
          .map(f => {val field = fieldFormat.read(f); field.name -> field}): _*)
      )
    }
  }
  implicit val cardFormat: RootJsonFormat[CatalogCard] = new RootJsonFormat[CatalogCard]
  {
    override def write(obj: CatalogCard): JsValue = JsObject(Map(
      "id" -> LongJsonFormat.write(obj.id),
      "records" -> JsArray(obj.records.map(recordFormat.write)),
      "coll" -> collectibleFormat.write(obj.coll)))

    override def read(json: JsValue): CatalogCard = {
      val c = CatalogCard(collectibleFormat.read(json.asJsObject.fields("coll")))
      val recs = RootJsArrayFormat.read(json.asJsObject.fields("records")).elements
        .map(recordFormat.read)
      c.records = recs
      c
    }
  }
}