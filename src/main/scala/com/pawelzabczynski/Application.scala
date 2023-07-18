package com.pawelzabczynski

import com.pawelzabczynski.grammar.tokenizer.Lexer
import com.pawelzabczynski.grammar.tokenizer.TokenizerError.InvalidTokenError

object Application {
  def main(args: Array[String]): Unit = {
    val expression =
      """if (10 < 5) {
        | 20
        |} else {
        | 50
        | }
        |""".stripMargin
    Lexer.lex(expression) match {
      case Right(lexemes) => lexemes.foreach(println)
      case Left(error: InvalidTokenError) => println(error.message)
    }

    //.toOption.get.foreach(println)
  }
}
