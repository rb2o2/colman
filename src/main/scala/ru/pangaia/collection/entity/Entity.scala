package ru.pangaia.collection.entity

import java.time.Instant

/**
  * The root of all persistent classes in the system
  * @author Roman Bortnikov &lt;rb2o2.dev@gmail.com&gt;
  */
trait Entity
{
  val id: Long = Entity.getAndInc
  val createdOn: Instant = Instant.now
  var modifiedOn: Option[Instant] = None
  val createdBy: User
  var modifiedBy: Option[User] = None
  var modifiedComment: Option[String] = None
}

/**
  * Used to get new [[ru.pangaia.collection.entity.Entity Entity]] id value
  * */
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

/**
  * Parent trait for every user type
  */
trait User extends Entity
{
  var name: String
  val login: String
  var password: String
  var email: String
}

/**
  * The superuser
  */
object RootAuthority extends User
{
  var name = "ROOT_AUTHORITY"
  override val login = "root"
  var password = "root"
  var email = "rb2o2.dev@gmail.com"
  override val createdBy: User = RootAuthority
}

/**
  * Trait for something having a name and a short description
  */
trait Named
{
  val name: String
  val description: String
}

