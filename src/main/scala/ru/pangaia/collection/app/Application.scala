package ru.pangaia.collection.app

import ru.pangaia.collection.entity.Collectible

import scala.annotation.tailrec
import scala.io.StdIn

class Application
{
  def main(args: Array[String]): Unit =
  {
    println("This is Colman, a simple collection manager\nPress Enter to continue...")
    io.StdIn.readLine()
    println("Menu____________________\nAdd a (C)ollection\nAdd a (F)ield\nAdd a ca(R)d\n(Q)uit")
    var choice = StdIn.readLine()
    @tailrec
    def loop(): Unit =
    {
      val choice = StdIn.readLine().substring(0,1).toLowerCase
      choice match
      {
        case "" => ()
        case "q" => ()
        case "c" => addCollection(); loop()
        case "f" => addField(); loop()
        case "r" => addCard(); loop()
      }

    }
    def addCollection(): Unit =
    {
      val name = StdIn.readLine("enter Collection name")
      //TODO
    }
    def addField(): Unit = ???
    def addCard(): Unit = ???
  }
}
