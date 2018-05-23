package ru.pangaia.collection.model

import ru.pangaia.collection.entity.{Named, PersistentEntity, User}

import scala.collection.mutable

/**
  * A type of things in a collection, i.e. book, postage stamp, coin etc.
  *
  * @param name see [[ru.pangaia.collection.entity.Named Named]]
  * @param description see [[ru.pangaia.collection.entity.Named Named]]
  * @param fields a mutable.Map of fields with field name as a key.
  *               See [[CardField CardField]]
  * @param user creator. Is written to the <code>createdBy</code> field
  */
case class Collectible(override val name: String,
                       override val description: String = "generic collection")
                      (implicit user: User) extends Named with PersistentEntity
{
  override val createdBy: User = user
  private var fields: mutable.Map[String, CardField] = mutable.LinkedHashMap()
  /**
    * Makes Vector of [[Record Records]]
    * from existing fields. See [[Record Record]]
    *
    * @return Vector of Records
    *
    */
//  def initRecordsVectorFromFields: Vector[Record] = fields.values.map((fld: CardField) => Record(fld)).toVector

  /**
    * Gets field of this Collectible by name wrapped in a Option
    *
    * @param fldName name of the field which is contained in field Map
    * @return Some(field) or None if not found by field name
    */
  def getField(fldName: String): Option[CardField] = fields.get(fldName)

  /**
    *
    * @return Vector of all the fields in this Collectible
    */
  def fieldsVector: Vector[CardField] = fields.values.toVector

  def getFields: Map[String, CardField] = fields.toMap
  /**
    * Adds field to this Collectible.
    * After a field is added, this Collectible is marked as modified by writing
    * timestamp, user who performed the addition and short comment into it.
    *
    * @param fld a field to add
    * @param user modifier. Is written to the <code>modifiedBy</code> field<br/>
    *
    */
  def addField(fld: CardField)(implicit user: User): Unit =
  {
    fields += (fld.name -> fld)

    updateWithComment(user, s"added field: $fld")

  }
}
