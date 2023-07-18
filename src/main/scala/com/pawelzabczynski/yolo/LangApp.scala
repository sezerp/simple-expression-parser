package com.pawelzabczynski.yolo

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec

object LangApp {
  def main(args: Array[String]): Unit = {
    val src =
      """
        |var x = 100
        |if(x > 10) {
        | print x
        | }
        |""".stripMargin
    run(src)
//    if (args.length > 1) {
//      System.exit(64)
//    } else if (args.length == 1) {
//      runScript(args(0))
//    } else {
//      runPrompt()
//    }
  }

  private def runScript(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(bytes, Charset.defaultCharset())
  }

  private def runPrompt(): Unit = {
    val isr = new InputStreamReader(System.in)
    val br  = new BufferedReader(isr)
    @tailrec
    def loop(): Unit = {
      print("> ")
      val line = br.readLine()
      if (line == null) ()
      else {
        run(line)
        loop()
      }
    }

    loop()
  }

  private def run(bytes: Array[Byte], cs: Charset): Unit = {
    ???
  }

  private def run(line: String): Unit = {
    val lexer  = new Scanner(line)
    val tokens = lexer.lex()

    println(line)
    tokens.foreach(println)
  }

}
