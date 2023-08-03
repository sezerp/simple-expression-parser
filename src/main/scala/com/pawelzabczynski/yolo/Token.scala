package com.pawelzabczynski.yolo

final case class Token(`type`: TokenType, lexeme: Option[String], literal: Option[Any], line: Int)

object Token {
  def apply(`type`: TokenType, line: Int): Token = {
    Token(`type`, None, None, line)
  }
}
