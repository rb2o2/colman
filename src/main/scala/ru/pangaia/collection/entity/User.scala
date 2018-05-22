package ru.pangaia.collection.entity

/**
  * Parent trait for every user type
  */
trait User extends PersistentEntity
{
  var name: String
  val login: String
  var password: String
  var email: String
}

