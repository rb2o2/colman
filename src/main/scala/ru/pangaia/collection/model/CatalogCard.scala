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

  private var ownFields: mutable.Map[String, CardField] = mutable.LinkedHashMap()

  def getOwnFields: mutable.Map[String, CardField] = ownFields

  def addField(f: CardField): Unit = {
    ownFields += (f.name -> f)
  }

  def getCommonFields: Map[String, CardField] = coll.getFields

//  override def toString: String = records.map((r: Record) =>
//  {
//    r.field.name + ": " + r.value
//  }).mkString("; ")
}
