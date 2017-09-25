package ru.pangaia.collection.entity

import java.sql.Timestamp
import java.time.Instant


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
trait User extends Entity

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
                        children: Seq[CategoryNode])
{
  def containsDeeper(v: String): Boolean =
  {
    index == v || children.exists(_.containsDeeper(v))
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
    val catTree = CategoryNode("root1", Cat("root", "some category"), List(
      CategoryNode("man", Cat("man", "a man"), List()),
      CategoryNode("woman", Cat("woman", "a woman"), List())))
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

