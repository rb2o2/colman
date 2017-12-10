package ru.pangaia.collection.entity

import java.io.{File, FileInputStream, InputStreamReader}
import java.sql.Timestamp
import java.time.Instant

import spray.json.{JsValue, RootJsonReader}

import scala.collection.generic.Growable
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import ru.pangaia.collection.entity._


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

//Test case-----------
object ACard
{
  val card : CatalogCard =
  {
//    val catTree = CategoryNode("root1", Cat("root", "some category"), ListBuffer(
//      CategoryNode("man", Cat("man", "a man"), ListBuffer()),
//      CategoryNode("woman", Cat("woman", "a woman"), ListBuffer())))
    val flds: Seq[CardField] = List(
      StringField("Title", "Заглавие"),
      StringField("Author", "Автор"),
      IntField("Pages", "Количество страниц").withRegex("[1-9][0-9]{0,3}"),
      ChoiceField("Periodicity", "", List("Книга", "Альманах", "Журнал", "Статья")),
//      TaxonField("Author's sex", "Sex of the author", catTree)
    )
    val book: Collectible = Collectible("BOOK",fields = flds)
    val card: CatalogCard = CatalogCard(book, List())
    card.records.foreach((r:Record) => r match
    {
      case Record(StringField("Title", _), _) => r.field.writeToRecord(r, "Властелин колец")
      case Record(StringField("Author", _), _) => r.field.writeToRecord(r, "Дж. Р. Р. Толкиен")
      case Record(ChoiceField("Periodicity", _, _), _) => r.field.writeToRecord(r, "Книга")
//      case Record(TaxonField("Author's sex", _, _), _) => r.field.writeToRecord(r, "man")
      case Record(IntField("Pages", _), _) => r.field.writeToRecord(r, "1020")
      case _ => ()
    })
    println(card)
    card
  }
}

