package collection.entity

import java.sql.Timestamp


trait Entity
{
  val id: Long
  val createdOn: Timestamp
  var modifiedOn: Timestamp
  val createdBy: User
  var modifiedBy: User
}
trait User extends Entity


//trait Collectible extends Named //with Entity
//{
//  val fields: Seq[CardField]
//}

case class Collectible(override val name: String,
                       override val description: String = "generic collection",
                       fields: Seq[CardField]) extends Named

trait Named
{
  val name: String
  val description: String
}

sealed trait CardField extends Named// with Entity
{
  def validate(s: String): Boolean
  def writeToRecord(r: Record, s: String): Unit
  def default: String
}
case class StringField(override val name: String,
                       override val description: String) extends CardField
{
  override def validate(s: String): Boolean = true

  override def writeToRecord(r: Record, s: String): Unit =
  {
    if (r.field == this && validate(s)) r.value = s
  }

  override def default: String = "--"
}
case class IntField(override val name: String,
                    override val description: String) extends CardField
{
  override def validate(s: String): Boolean = s.matches("\\d+")

  override def writeToRecord(r: Record, s: String): Unit =
  {
    if (r.field == this && validate(s))
    {
      r.value = s
    }
  }

  override def default: String = "0"
}
case class ChoiceField(override val name: String,
                       override val description: String,
                       possibleChoices: Seq[String]) extends CardField
{
  override def validate(s: String): Boolean = possibleChoices.contains(s)

  override def writeToRecord(r: Record, s: String): Unit =
  {
    if (r.field == this && validate(s)) r.value = s
  }

  override def default: String = possibleChoices.head
}

//trait Record //extends Entity
//{
//  val field: CardField
//  var value: String
//}
case class Record(field: CardField, var value: String) //extends Record

//trait CatalogCard //extends Entity
//{
//  val coll: Collectible
//  def getFields: Seq[CardField] = coll.fields
//  val records: Seq[Record]
//}
case class CatalogCard(coll: Collectible, var records: List[Record]) //extends CatalogCard
{
//  override def getRecords: Seq[Record] = records

  records = coll.fields.map((fld: CardField) => Record(fld, fld.default)).toList

  override def toString: String = records.map((r: Record)=>
  {
    r.field.name +": "+  r.value
  }).mkString("; ")
}

object ACard
{
  def card: CatalogCard =
  {
    val flds: Seq[CardField] = List(
      StringField("Title", "title of the book"),
      StringField("Author", "author of the book"),
      IntField("Pages", "number of pages"),
      ChoiceField("Periodicity", "", List("Book", "Almanac", "Journal", "Paper"))
    )
    val book: Collectible = Collectible("BOOK",fields = flds)
    val card: CatalogCard = CatalogCard(book, List())
    card.records.foreach((r:Record) =>
      r match {
        case Record(StringField("Title",_), _) => r.field.writeToRecord(r, "LOTR")
        case Record(StringField("Author",_), _) => r.field.writeToRecord(r, "Tolkien")
        case Record(ChoiceField("Periodicity",_,_), _) => r.field.writeToRecord(r, "Book")
        case _ => ()
      }
    )
    println(card)
    card
  }
}

