package com.pawelzabczynski.grammar.tokenizer

trait TokenizerError extends Exception

object TokenizerError {
  final case class InvalidTokenError(line: Int, column: Int, message: String) extends TokenizerError
  object  InvalidTokenError {
    def apply(line: Int, column: Int, token: Char): InvalidTokenError = {
      new InvalidTokenError(line, column, s"The token '$token' is not allowed. Line: $line, Col: $column")
    }
  }
}
