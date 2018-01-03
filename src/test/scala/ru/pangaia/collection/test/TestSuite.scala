package ru.pangaia.collection.test

import org.scalatest.FunSuite
import ru.pangaia.collection.entity._

import scala.collection.mutable

class TestSuite extends FunSuite
{
  object ACard
  {
    implicit val user: User = RootAuthority
    val card: CatalogCard =
    {
      val periodicityChoices = Set("Книга", "Альманах", "Журнал", "Статья")
      val flds: mutable.Map[String, CardField] = mutable.Map(
        "Title" -> StringField("Title", "Заглавие"),
        "Author" -> StringField("Author", "Автор"),
        "Pages" -> IntField("Pages", "Количество страниц"),
        "Periodicity" -> ChoiceField("Periodicity", "", periodicityChoices)
      )
      val book: Collectible = Collectible("BOOK", fields = flds)
      val card: CatalogCard = CatalogCard(book)
      Map(
        "Title" -> "Властелин колец",
        "Author" -> "Дж. Р. Р. Толкиен",
        "Periodicity" -> "Книга",
        "Pages" -> "1020").foreach((a) => card.writeToFieldRecord(card.coll.getField(a._1).get)( a._2))
      println(card)
      card
    }
  }

  test("true is true")
  {
    assert(true)
  }

  test("createdBy is RootAuthority")
  {
    assert {ACard.card.createdBy == RootAuthority}
  }
}
