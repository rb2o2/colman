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
  var modifiedOn: Option[Timestamp] = None
  val createdBy: User
  var modifiedBy: Option[User]
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
{
  var name: String
  val login: String
  var password: String
  var email: String
}
object RootAuthority extends User
{
  override var name = "ROOT_AUTHORITY"
  override val login = "root"
  override var password = "root"
  override var email = "rb2o2.dev@gmail.com"
  override val createdBy: User = RootAuthority
  override var modifiedBy: Option[User] = None
}


trait Named
{
  val name: String
  val description: String
}

//Test case-----------
object ACard
{
  implicit val user: User = RootAuthority
  val card : CatalogCard =
  {
//    val catTree = CategoryNode("root1", Cat("root", "some category"), ListBuffer(
//      CategoryNode("man", Cat("man", "a man"), ListBuffer()),
//      CategoryNode("woman", Cat("woman", "a woman"), ListBuffer())))
    val flds: Map[String, CardField] = Map(
      "Title" -> StringField("Title", "Заглавие"),
      "Author" -> StringField("Author", "Автор"),
      "Pages" -> IntField("Pages", "Количество страниц").withRegex("[1-9][0-9]{0,3}"),
      "Periodicity" -> ChoiceField("Periodicity", "", List("Книга", "Альманах", "Журнал", "Статья")),
//      TaxonField("Author's sex", "Sex of the author", catTree)
    )
    val book: Collectible = Collectible("BOOK",fields=flds)
    val card: CatalogCard = CatalogCard(book, Array())
    card.records.foreach((r:Record) => r match
    {
      case Record(StringField("Title", _)) => r.field.writeToRecord(r, "Властелин колец")
      case Record(StringField("Author", _)) => r.field.writeToRecord(r, "Дж. Р. Р. Толкиен")
      case Record(ChoiceField("Periodicity", _, _)) => r.field.writeToRecord(r, "Книга")
//      case Record(TaxonField("Author's sex", _, _)) => r.field.writeToRecord(r, "man")
      case Record(IntField("Pages", _)) => r.field.writeToRecord(r, "1020")
      case _ => ()
    })
//    card.coll.fields.values.foreach((r:CardField) => r match
//    {
//      case StringField("Title", _) => r.writeToRecord(Record(), "Властелин колец")
//      case StringField("Author", _) => r.field.writeToRecord(r, "Дж. Р. Р. Толкиен")
//      case ChoiceField("Periodicity", _, _) => r.field.writeToRecord(r, "Книга")
////      case Record(TaxonField("Author's sex", _, _), _) => r.field.writeToRecord(r, "man")
//      case IntField("Pages", _) => r.field.writeToRecord(r, "1020")
//      case _ => ()
//    })

    println(card)
    card
  }
}

