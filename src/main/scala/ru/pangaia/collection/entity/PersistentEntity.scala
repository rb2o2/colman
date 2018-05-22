package ru.pangaia.collection.entity

import java.time.Instant

/**
  * The root of all persistent classes in the system
  * @author Roman Bortnikov &lt;rb2o2.dev@gmail.com&gt;
  */
trait PersistentEntity
{
  val id: Long = PersistentEntity.getAndInc
  val createdOn: Instant = Instant.now
  var modifiedOn: Option[Instant] = None
  val createdBy: User
  var modifiedBy: Option[User] = None
  var modifiedComment: Option[String] = None
  def updateWithComment(user:User, comment: String): Unit = {
    modifiedBy = Some(user)
    modifiedComment = Some(comment)
    modifiedOn = Some(Instant.now())
  }
}

/**
  * Used to get new [[ru.pangaia.collection.entity.PersistentEntity Entity]] id value
  * */
object PersistentEntity
{
  private var id_counter:Long = 0L

  def getAndInc:Long =
  synchronized {
    val c = 0 + id_counter
    id_counter += 1
    c
  }
}


