package ru.pangaia.collection.model

import java.time.Instant

import ru.pangaia.collection.entity._

import scala.collection.generic.Growable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

case class CatalogCard(coll: Collectible)(implicit user: User) extends PersistentEntity
{
  override val createdBy: User = user
  private var vec = coll.initRecordsVectorFromFields
  def records: Vector[Record] =
    {
      vec.filter(r => coll.fields.values.toVector.count(f => r.field == f) == 1) ++
        //picks records for which one field exist in collectible in case a field was removed
        coll.fields.values.toVector.filterNot((f) => vec.exists(r => r.field == f))
          .map((f) => Record(f))
      //creates new records for fields which has no corresponding records in case a field was created
      //TODO optimize
    }

  def records_= (recs: Vector[Record])(implicit user: User): Unit =
  {
    vec = recs

    updateWithComment(user, "records set")
  }

  def getRecordValueByFieldName(fldName: String): Option[String] =
  {
    val field: Option[CardField] = coll.getField(fldName)
    field match
    {
      case None => None
      case Some(f) => records.find((r) => r.field == f).map((r) => r.value)
    }
  }

  def writeToFieldRecord(field: CardField)(value: String): Unit =
  {
    records.find(r => r.field == field).foreach ((r) =>
    {
      field.writeToRecord(r, value)
    })
  }

  override def toString: String = records.map((r: Record) =>
  {
    r.field.name + ": " + r.value
  }).mkString("; ")
}
