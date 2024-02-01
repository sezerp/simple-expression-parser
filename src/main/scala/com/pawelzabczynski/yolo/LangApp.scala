package com.pawelzabczynski.yolo

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec

object LangApp {
  def main(args: Array[String]): Unit = {
    val src =
      """
        |// some comment
        |var x = .2
        |var y = "some string"
        |if(x >= 10 and y == "some string") {
        | print x
        | }
        |""".stripMargin
    run(src)

    val expression = Binary(
      Unary(Token(TokenType.Minus, Some("-"), None, 1), Literal(NumericVal(123))),
      Token(TokenType.Star, Some("*"), None, 1),
      Grouping(Literal(NumericVal(45.67)))
    )

    println(AstPrinter.print(expression))
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
