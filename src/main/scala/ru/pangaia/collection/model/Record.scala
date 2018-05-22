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

  /**
    * setter for the value of this Record
    * @param value some string conforming to the <code>field</code> semantics
    * @param user modifier. Is written to <code>modifiedBy</code> field
    */
  def value_=(value: ValueType)(implicit user: User): Try[Unit]

  def value: Option[ValueType]
}

case class StringRecord(field: StringField)(implicit user: User) extends Record(field)(user)
{
  override type ValueType = String
  private var _value: Option[ValueType] = None

  /**
    * setter for the value of this Record
    *
    * @param value some string conforming to the <code>field</code> semantics
    * @param user  modifier. Is written to <code>modifiedBy</code> field
    */
  override def value_=(value: ValueType)(implicit user: User): Try[Unit] = Try {
    _value = Some(value)
  }

  override def value: Option[String] = _value
}

case class IntRecord(field: IntField)(implicit user: User) extends Record(field)(user)
{
  override type ValueType = Int
  private var _value: Option[ValueType] = None

  /**
    * setter for the value of this Record
    *
    * @param value some string conforming to the <code>field</code> semantics
    * @param user  modifier. Is written to <code>modifiedBy</code> field
    */
  override def value_=(value: ValueType)(implicit user: User): Try[Unit] = Try {
    _value = Some(value)
  }

  override def value: Option[ValueType] = _value
}

case class BooleanRecord(field: BooleanField)(implicit user: User) extends Record(field)(user)
{
  override type ValueType = Boolean

  private var _value: Option[ValueType] = None

  /**
    * setter for the value of this Record
    *
    * @param value some string conforming to the <code>field</code> semantics
    * @param user  modifier. Is written to <code>modifiedBy</code> field
    */
  override def value_=(value: ValueType)(implicit user: User): Try[Unit] = Try {
    _value = Some(value)
  }

  override def value: Option[Boolean] = _value
}