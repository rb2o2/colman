package ru.pangaia.collection.model

import ru.pangaia.collection.entity.{PersistentEntity, User}

import scala.util.Try

/**
  * Data written to a catalog card, into an associated field.
  *
  * @param field an assosiated CardField of a Collectible
  * @param user creator. Is written to <code>createdBy</code> field
  */
sealed abstract class Record(field: CardField)(implicit user: User) extends PersistentEntity
{
  type ValueType
  override val createdBy: User = user
  def getField: CardField = field
  def getDefault: ValueType

  /**
    * setter for the value of this [[Record]]
    *
    * @param value some string conforming to the <code>field</code> semantics
    * @param user  modifier. Is written to <code>modifiedBy</code> field
    */
  def value_=(value: ValueType)(implicit user: User): Try[Unit]

  def value: Option[ValueType]
}

case class StringRecord(field: StringField)(implicit user: User) extends Record(field)(user)
{
  override type ValueType = String
  private var _value: Option[ValueType] = None

  override def value_=(value: ValueType)(implicit user: User): Try[Unit] = Try {
    updateWithComment(user, s"value changed from: ${_value}" + s" to: $value")
    _value = Some(value)
  }


  override def getDefault: String = "--"

  override def value: Option[String] = _value
}

case class IntRecord(field: IntField)(implicit user: User) extends Record(field)(user)
{
  override type ValueType = Int
  private var _value: Option[ValueType] = None

  override def value_=(value: ValueType)(implicit user: User): Try[Unit] = Try {
    updateWithComment(user, s"value changed from: ${_value}" + s" to: $value")
    _value = Some(value)
  }


  override def getDefault: Int = 0

  override def value: Option[ValueType] = _value
}

case class BooleanRecord(field: BooleanField)(implicit user: User) extends Record(field)(user)
{
  override type ValueType = Boolean

  private var _value: Option[ValueType] = None

  override def value_=(value: ValueType)(implicit user: User): Try[Unit] = Try {
    updateWithComment(user, s"value changed from: ${_value}" + s" to: $value")
    _value = Some(value)
  }


  override def getDefault: Boolean = false

  override def value: Option[Boolean] = _value
}