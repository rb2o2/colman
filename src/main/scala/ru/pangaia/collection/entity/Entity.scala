package ru.pangaia.collection.entity

import java.io.{File, FileInputStream, InputStreamReader}
import java.sql.Timestamp
import java.time.Instant

import spray.json.{JsValue, RootJsonReader}

import scala.collection.generic.Growable
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


trait Entity
{
  val id: Long = Entity.getAndInc
  val createdOn: Timestamp = Timestamp.from(Instant.now)
//  var modifiedOn: Timestamp
//  val createdBy: User
//  var modifiedBy: User
}
object Entity
{
  var id_counter:Long = 0L

  def getAndInc:Long =
  {
    val c = 0 + id_counter
    id_counter += 1
    c
  }
}
trait User extends Named with Entity

trait Named
{
  val name: String
  val description: String
}
case class Collectible(override val name: String,
                       override val description: String = "generic collection",
                       fields: Seq[CardField]) extends Named with Entity


sealed trait CardField extends Named with Entity
{
  def validate(s: String): Boolean = s.matches(pattern)
  def writeToRecord(r: Record, s: String): Unit =
  {
    if (r.field == this && validate(s)) r.value = s
  }
  val default: String
  var pattern: String
  def withRegex(re: String): CardField = {pattern = re; this}
}

case class StringField(override val name: String,
                       override val description: String) extends CardField
{
  override val default: String = "--"

  var pattern = ".*"
}
case class IntField(override val name: String,
                    override val description: String) extends CardField
{
  override val default: String = "0"

  var pattern = "[1-9][0-9]*"
}
case class BooleanField(override val name: String,
                        override val description: String) extends CardField
{
  override val default: String = "false"

  override var pattern: String = "true|false"
}
case class ChoiceField(override val name: String,
                       override val description: String,
                       possibleChoices: Seq[String]) extends CardField
{
  override def validate(s: String): Boolean = possibleChoices.contains(s)

  override val default: String = possibleChoices.head

  var pattern = ".*"
}

case class CategoryNode(index: String,
                        value: Cat,
                        var children: collection.mutable.Seq[CategoryNode] with Growable[CategoryNode])
{
  def containsDeeper(v: String): Boolean =
  {
    index == v || children.exists(_.containsDeeper(v))
  }
}
case class InverseCategoryNode(index: String,
                               value: Cat,
                               parent: InverseCategoryNode)
case object InverseCategoryNode
{
//  implicit val reader: RootJsonReader[Map[String,InverseCategoryNode]] = new RootJsonReader[Map[String, InverseCategoryNode]]
//  {
//    override def read(json: JsValue): Map[String, InverseCategoryNode] =
//    {
//      val a: Map[String, InverseCategoryNode] =
//        json.asJsObject.fields.values.map((js: JsValue) =>
//        {
//          val ob = js.asJsObject;
//          (ob.fields("name").toString,
//            InverseCategoryNode(
//              ob.fields("name").toString(),
//              Cat(ob.fields("name").toString(),ob.fields("desc").toString()),
//
//            ))
//        }).toMap[String, InverseCategoryNode]
//    }
//  }
  def readFromJson(filepath: String): Map[String, InverseCategoryNode] =
  {
    val fileIS = new InputStreamReader(new FileInputStream(filepath))
    try
    {
      val cbuff: Array[Char] = new Array(4096)
      val sb: mutable.StringBuilder = new StringBuilder()
      while (fileIS.read(cbuff) != -1)
      {
        sb.append(cbuff)
      }
      ???
    }
    catch
    {
      case ex: Exception => fileIS.close(); ???
    }
    finally
    {
      fileIS.close()
    }
  }
}
case object CategoryNode
{
  def rootFromInverse(nodes: Map[String,InverseCategoryNode]): CategoryNode =
  {
    val root = CategoryNode("UDC", Cat("root","Корень УДК"), ListBuffer[CategoryNode]())
//    def addBranch(node: InverseCategoryNode, catnode: CategoryNode) =
//    {
//      if (node.parent.index == "UDC")
//        root.children = CategoryNode(node.index, node.value, ListBuffer()) +: root.children
//    }
    val nodesStraight = collection.mutable.Map[String, CategoryNode]()
    for ((ind, node) <- nodes)
    {
      nodesStraight += (ind -> CategoryNode(node.index, node.value, ListBuffer()))
    }
    nodesStraight += ("UDC" -> root)
    for ((ind, node) <- nodes)
    {
      nodesStraight(node.parent.index).children += nodesStraight(ind)
    }
    root
  }
}

case class TaxonField(override val name: String,
                      override val description: String,
                      root: CategoryNode) extends CardField with Entity
{
  override def validate(s: String): Boolean = root containsDeeper s

  override val default: String = root.index

  var pattern: String = ".*"
}

case class Record(field: CardField, var value: String) extends Entity

case class CatalogCard(coll: Collectible, var records: List[Record]) extends Entity
{
  records = coll.fields.map((fld: CardField) => Record(fld, fld.default)).toList

  override def toString: String = records.map((r: Record)=>
  {
    r.field.name +": "+  r.value
  }).mkString("; ")
}
case class Cat(override val name: String, override val description: String) extends Named with Entity

//Test case-----------
object ACard
{
  val card : CatalogCard =
  {
    val catTree = CategoryNode("root1", Cat("root", "some category"), ListBuffer(
      CategoryNode("man", Cat("man", "a man"), ListBuffer()),
      CategoryNode("woman", Cat("woman", "a woman"), ListBuffer())))
    val flds: Seq[CardField] = List(
      StringField("Title", "title of the book"),
      StringField("Author", "author of the book"),
      IntField("Pages", "number of pages").withRegex("[1-9][0-9]{0,3}"),
      ChoiceField("Periodicity", "", List("Book", "Almanac", "Journal", "Paper")),
      TaxonField("Author's sex", "Sex of the author", catTree)
    )
    val book: Collectible = Collectible("BOOK",fields = flds)
    val card: CatalogCard = CatalogCard(book, List())
    card.records.foreach((r:Record) => r match
    {
      case Record(StringField("Title", _), _) => r.field.writeToRecord(r, "LOTR")
      case Record(StringField("Author", _), _) => r.field.writeToRecord(r, "Tolkien")
      case Record(ChoiceField("Periodicity", _, _), _) => r.field.writeToRecord(r, "Book")
      case Record(TaxonField("Author's sex", _, _), _) => r.field.writeToRecord(r, "man")
      case _ => ()
    })
    println(card)
    card
  }
}

