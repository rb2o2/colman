package ru.pangaia.collection.model

import ru.pangaia.collection.entity.{Named, PersistentEntity, User}

import scala.collection.generic.Growable
import scala.util.{Failure, Success, Try}


sealed trait CardField extends Named with PersistentEntity
{
  /**
    * Type of the associated record value
    */
  type recordType
  val default: recordType

  /**
    * Writes a string to specific record. String must conform to this CardField and
    * record must be associated with this CardField
    *
    * @param r record to write to.
    * @param s raw string value
    * @param user is passed to record value setter
    * @return Try with possible IllegalArgumentException in case string is invalid for this CardField or
    *         r is associated with CardField other than this
    *
    */
  def writeToRecord(r: Record, s: String)(implicit user: User): Try[Unit] =
  {
    Try {
      if (r.field != this)  throw new IllegalArgumentException(s"wrong record")
      else if (! valid(s)) throw new IllegalArgumentException(s"value $s is invalid")
      else
        r.value = s
    }
  }

  def valid(s: String): Boolean

  /**
    * Reads value from record
    * @param record record to read a value from
    * @return value of proper type wrapped in Option
    */
  def read(record: Record): Option[recordType]
}

case class StringField(override val name: String,
                       override val description: String)
                      (implicit user: User) extends CardField
{
  require(!name.isEmpty, "Name must not be empty")
  override type recordType = String
  override val createdBy: User = user
  override val default: String = "--"
  private var pattern = ".*"

  override def valid(s: String): Boolean = true

  override def read(record: Record) = Some(record.value)
}

case class IntField(override val name: String,
                    override val description: String)
                   (implicit user: User) extends CardField
{
  override type recordType = Int
  override val createdBy: User = user
  override val default = 0
  private var pattern = "[1-9][0-9]*"

  override def valid(s: String): Boolean = Try {s.toInt} match
  {
    case a: Success[_] => true
    case a: Failure[_] => false
  }

  override def read(record: Record) = Some(record.value.toInt)
}

case class BooleanField(override val name: String,
                        override val description: String)
                       (implicit user: User) extends CardField
{
  override type recordType = Boolean
  override val createdBy: User = user
  override val default = false

  override def valid(s: String): Boolean = s.matches("true|false")

  override def read(record: Record): Option[recordType] = record.value match
  {
    case "true" => Some(true)
    case "false" => Some(false)
    case _ => None
  }
}

case class ChoiceString(choices: Set[String], choice: String)
{
  require(choices.contains(choice), "Choice is not in possible choices set")
  override def toString: String = choice
}

case class ChoiceField(override val name: String,
                       override val description: String,
                       possibleChoices: Set[String])(implicit user: User) extends CardField
{
  override type recordType = ChoiceString
  override val createdBy: User = user
  override val default: ChoiceString = ChoiceString(possibleChoices, possibleChoices.head)

  override def read(record: Record) = Some(ChoiceString(possibleChoices, record.value))

  override def valid(s: String): Boolean = possibleChoices.contains(s)
}

case class Cat(index: String,
               override val name: String,
               override val description: String)
              (implicit user: User) extends Named with PersistentEntity
{
  override val createdBy: User = user

  override def toString: String = index
}

case class CategoryNode(index: String,
                        value: Cat,
                        var children: collection.mutable.Seq[CategoryNode] with Growable[CategoryNode])
{
  def containsDeeper(v: String): Boolean =
  {
    index == v || children.exists(_.containsDeeper(v))
  }

  def findByIndex(ind: String): Option[Cat] =
  {
    if (value.index == ind)
      Some(value)
    else if (children.isEmpty)
      None
    else
    {
      val m = children.map(c => c.findByIndex(ind))
      if (m.isEmpty) None
      else m.head
    }
  }

  def containsCat(c: Cat): Boolean =
  {
    if (value == c) true
    else children.exists(cn => cn.containsCat(c))
  }
}

case class TaxonField(override val name: String,
                      override val description: String,
                      root: CategoryNode)(implicit user: User) extends CardField
{
  override type recordType = Cat
  override val createdBy: User = user
  override val default: Cat = root.value
  private var pattern: String = ".*"

  override def valid(s: String): Boolean = root.containsDeeper(s)

  override def read(record: Record): Option[recordType] = root.findByIndex(record.value)
}
