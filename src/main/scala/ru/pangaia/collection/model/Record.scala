package ru.pangaia.collection.model

import ru.pangaia.collection.entity.{PersistentEntity, User}

/**
  * Data written to a catalog card, into an associated field.
  *
  * @param field an assosiated CardField of a Collectible
  * @param user creator. Is written to <code>createdBy</code> field
  */
case class Record(field: CardField)(implicit user: User) extends PersistentEntity
{
  override val createdBy: User = user
  private var valu: String = field.default.toString
  def getField: CardField = field

  /**
    * setter for the value of this Record
    * @param value some string conforming to the <code>field</code> semantics
    * @param user modifier. Is written to <code>modifiedBy</code> field
    */
  def value_=(value: String)(implicit user: User): Unit =
  {
    this.valu = value

    updateWithComment(user, s"value changed from: $valu to: $value")
  }

  def value: String = valu
}
