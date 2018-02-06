package ru.pangaia.collection

/**
  * Created by oneuro on 05.10.17.
  */
import ru.pangaia.collection.entity._

import scala.collection.mutable

trait Repository[T <: Entity]
{
  def getById(id: Long) : Option[T]

  def add(entity: T): Unit

  def getAll: Seq[T]
}

class CardFieldRepository extends Repository[CardField]
{
  private val values = mutable.Map.empty[Long, CardField]

  override def getById(id: Long): Option[CardField] = values.get(id)

  override def add(entity: CardField): Unit = {values.put(entity.id, entity);()}

  override def getAll: Seq[CardField] = values.values.toSeq
}

class CatalogCardRepository extends Repository[CatalogCard]
{
  private val values = mutable.Map.empty[Long, CatalogCard]

  override def getById(id: Long): Option[CatalogCard] = values.get(id)

  override def add(entity: CatalogCard): Unit = {values.put(entity.id, entity);()}

  override def getAll: Seq[CatalogCard] = values.values.toSeq
}

class CollectionRepository extends Repository[Collection]
{
  private val values = mutable.Map.empty[Long, Collection]

  override def getById(id: Long): Option[Collection] = values.get(id)

  override def add(entity: Collection): Unit = {values.put(entity.id, entity);()}

  override def getAll: Seq[Collection] = values.values.toSeq
}

class CollectibleRepository extends Repository[Collectible]
{
  private val values = mutable.Map.empty[Long, Collectible]

  override def getById(id: Long): Option[Collectible] = values.get(id)

  override def add(entity: Collectible): Unit = {values.put(entity.id, entity);()}

  override def getAll: Seq[Collectible] = values.values.toSeq
}

class UserRepository extends Repository[User]
{
  case class ColmanUser(private val name_ : String,
                        private val login_ : String,
                        private val password_ : String,
                        private val email_ : String)(implicit val user: User) extends User {
    override var name: String = name_
    override val login: String = login_
    override var password: String = password_
    override var email: String = email_
    override val createdBy: User = user
  }

  private val values = {
    implicit val user: User = RootAuthority
    mutable.Map[Long, User](
      -1L -> RootAuthority,
      0L -> ColmanUser("Roman", "roman", "xyz", "roman.bortnikov@gmail.com")
    )
  }

  override def getById(id: Long): Option[User] = values.get(id)

  override def add(entity: User): Unit = {values.put(entity.id, entity); ()}

  def getRoot: User = getById(-1).get

  override def getAll: Seq[User] = values.values.toSeq
}

class RecordRepository extends Repository[Record]
{
  private val values = mutable.Map[Long, Record]()

  override def getById(id: Long): Option[Record] = values.get(id)

  override def add(entity: Record): Unit = {values.put(entity.id, entity); ()}

  override def getAll: Seq[Record] = values.values.toSeq
}

class DB {
}
