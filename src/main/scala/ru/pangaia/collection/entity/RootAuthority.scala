package ru.pangaia.collection.entity

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

