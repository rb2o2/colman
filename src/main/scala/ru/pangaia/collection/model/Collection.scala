package ru.pangaia.collection.model

import ru.pangaia.collection.entity.{Named, PersistentEntity, User}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A set of things representing a Collectible type gathered together.
  *
  * @param coll Collectible of every thing in this Collection
  * @param name name of this Collection
  * @param description short description of this Collection
  * @param user creator. Is written to <code>createdBy</code> field
  */
case class Collection(coll: Collectible,
                      override val name: String,
                      override val description: String)
                     (implicit user: User) extends Named with PersistentEntity
{
  override val createdBy: User = user
  /**
    * mutable collection of CatalogCards
    */
  val list: mutable.Buffer[CatalogCard] = new ArrayBuffer[CatalogCard]()

  /**
    * Adds a CatalogCard in this Collection. After a CatalogCard is added, this Collection is marked as modified by writing
    * timestamp, user who performed the addition and short comment into it.
    * @param c a catalog card of the thing being added. <code>c.coll</code> must be equal to <code>this.coll</code>
    * @param user modifier. Is written to <code>modifiedBy</code> field
    */
  def add(c: CatalogCard)(implicit user: User): Unit =
  {
    require(c.coll == coll)
    list += c

    updateWithComment(user, s"Added card $c to collection" )
  }

  /**
    * Removes a CatalogCard from this Collection. After removal, this Collection is marked as modified (see
    * [[Collection#add add]]).
    *
    * @param c a catalog card of the thing being removed
    * @param user modifier. Is written to <code>modifiedBy</code> field
    */
  def remove(c: CatalogCard)(implicit user: User): Unit =
  {
    list -= c

    updateWithComment(user, s"Removed card $c from collection")
  }
}
