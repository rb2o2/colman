package ru.pangaia.collection.app

import ru.pangaia.collection.entity._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

sealed trait MenuChoice
sealed trait MainMenuChoice extends MenuChoice

object NewCollection extends MainMenuChoice
object NewField extends MainMenuChoice
object NewCard extends MainMenuChoice
object Quit extends MainMenuChoice

sealed trait NewFieldChoice

object NewStringField extends NewFieldChoice
object NewIntField extends NewFieldChoice
object NewBooleanField extends NewFieldChoice
object Back extends NewFieldChoice
object UnknownChoice extends MenuChoice with MainMenuChoice with NewFieldChoice


object Application
{
  def main(args: Array[String]): Unit =
  {
    implicit val rootUser: User = RootAuthority
    val collections = mutable.Buffer[Collection]()
    val coll = Collectible("BOOK",fields = mutable.Map())
    println("This is Colman, a simple collection manager\nPress Enter to continue...")
    io.StdIn.readLine()

    @tailrec
    def loop(): Unit =
    {
      def input(prompt: String) = StdIn.readLine(prompt).substring(0,1).toLowerCase

      def mainMenuChoice: MainMenuChoice =
        input("Menu____________________\nAdd a (C)ollection\nAdd a (F)ield\nAdd a ca(R)d\n(Q)uit")
        match
      {
        case "q" => Quit
        case "c" => NewCollection
        case "f" => NewField
        case "r" => NewCard
        case _ => UnknownChoice
      }

      def fieldChoice: NewFieldChoice =
        input("Choose field data type: \n1 - String\n2 - Integer\n3 - Boolean(yes/no)")
        match
        {
          case "1" => NewStringField
          case "2" => NewIntField
          case "3" => NewBooleanField
          case "b" => Back
          case _ => UnknownChoice
        }

      def addStringField(): Unit =
      {
        val name = StdIn.readLine("Enter field name")
        val desc = StdIn.readLine("Enter field description")
        val fld = StringField(name, desc)
        coll.addField(fld)
      }

      def addIntField(): Unit =
      {
        val name = StdIn.readLine("Enter field name")
        val desc = StdIn.readLine("Enter field description")
        val fld = IntField(name, desc)
        coll.addField(fld)
      }

      def addBooleanField(): Unit =
      {
        val name = StdIn.readLine("Enter field name")
        val desc = StdIn.readLine("Enter field description")
        val fld = BooleanField(name, desc)
        coll.addField(fld)
      }

      def addCollection(): Unit =
      {
        val name = StdIn.readLine("Enter Collection name")
        val descr = StdIn.readLine("Enter Collection description")
        val collection = Collection(coll, name, descr)

        collections += collection
      }

      mainMenuChoice match
      {
          case UnknownChoice =>
          {
            loop()
          }
          case NewCollection =>
          {
            addCollection()
            loop()
          }
          case NewField =>
          {
            fieldChoice match
            {
              case NewStringField => addStringField()
              case UnknownChoice => ()
              case NewIntField => addIntField()
              case NewBooleanField => addBooleanField()
              case Back => ()
            }
            loop()
          }
          case NewCard => loop()
          case Quit => ()
      }
    }




    def addCard(): Unit = ???


    loop()
  }
}
