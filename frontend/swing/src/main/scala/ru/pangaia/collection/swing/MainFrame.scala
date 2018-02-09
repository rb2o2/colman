package ru.pangaia.collection.swing

import javax.swing.JFrame

class MainFrame extends JFrame
{
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  setSize(640, 480)
  setLocation(750, 500)
  setResizable(true)
  setTitle("ColMan v 0.1")
  setVisible(true)
}

object MainFrame {
  def main(args: Array[String]): Unit = {
    new MainFrame()
  }
}
