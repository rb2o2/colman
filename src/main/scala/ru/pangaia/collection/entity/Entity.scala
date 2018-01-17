package ru.pangaia.collection.entity

import java.sql.Timestamp
import java.time.Instant

trait Entity
{
  val id: Long = Entity.getAndInc
  val createdOn: Timestamp = Timestamp.from(Instant.now)
  var modifiedOn: Option[Timestamp] = None
  val createdBy: User
  var modifiedBy: Option[User] = None
  var modifiedComment: Option[String] = None
}

object Entity
{
  private var id_counter:Long = 0L

  def getAndInc:Long =
  synchronized {
    val c = 0 + id_counter
    id_counter += 1
    c
  }
}

trait User extends Entity
{
  var name: String
  val login: String
  var password: String
  var email: String
}

object RootAuthority extends User
{
  var name = "ROOT_AUTHORITY"
  override val login = "root"
  var password = "root"
  var email = "rb2o2.dev@gmail.com"
  override val createdBy: User = RootAuthority
}

trait Named
{
  val name: String
  val description: String
}

