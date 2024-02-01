package com.pawelzabczynski.grammar.tokenizer

trait TokenizerError extends Exception { self: Product =>
  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(this.getClass.getSimpleName).append("(")
    self.productIterator.foldLeft(sb) {
      case (acc, v) => acc.append(v).append(", ")
    }
      .result()
      .stripSuffix(", ") + ")"

  }
}

object TokenizerError {
  final case class InvalidTokenError(line: Int, column: Int, message: String) extends TokenizerError
  object  InvalidTokenError {
    def apply(line: Int, column: Int, token: Char): InvalidTokenError = {
      new InvalidTokenError(line, column, s"The token '$token' is not allowed. Line: $line, Col: $column")
    }
  }
}
