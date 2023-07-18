package com.pawelzabczynski.yolo

import com.pawelzabczynski.yolo.Scanner.Result

class Scanner(source: String) {
  def lex(): List[Token] = {
    def loop(current: Int, line: Int, acc: List[Token]): List[Token] = {
      if (isEnd(current)) {
        acc
      } else {
        val token = next(current)
        token match {
          case Grammar.LeftParen =>
            val nextAcc = Token(TokenType.LeftParen, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.RightParen =>
            val nextAcc = Token(TokenType.RightBrace, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.LeftBrace =>
            val nextAcc = Token(TokenType.LeftBrace, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.RightBrace =>
            val nextAcc = Token(TokenType.RightBrace, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.Comma =>
            val nextAcc = Token(TokenType.Coma, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.Dot =>
            val nextAcc = Token(TokenType.Dot, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.Minus =>
            val nextAcc = Token(TokenType.Minus, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.Plus =>
            val nextAcc = Token(TokenType.Plus, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.Semicolon =>
            val nextAcc = Token(TokenType.Semicolon, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.Star =>
            val nextAcc = Token(TokenType.Star, line) :: acc
            loop(current + 1, line, nextAcc)
          // two character tokens
          case Grammar.Bang =>
            val nextAcc =
              if (matchNext(current, Grammar.Equal)) Token(TokenType.BangEqual, line) :: acc
              else Token(TokenType.Bang, line) :: acc

            loop(current + 1, line, nextAcc)
          case Grammar.Equal =>
            val nextAcc =
              if (matchNext(current, Grammar.Equal)) Token(TokenType.EqualEqual, line) :: acc
              else Token(TokenType.Equal, line) :: acc

            loop(current + 1, line, nextAcc)
          case Grammar.Less =>
            val nextAcc =
              if (matchNext(current, Grammar.Equal)) Token(TokenType.LessEqual, line) :: acc
              else Token(TokenType.Less, line) :: acc
            loop(current + 1, line, nextAcc)
          case Grammar.Greater =>
            val nextAcc =
              if (matchNext(current, Grammar.Equal)) Token(TokenType.GreaterEqual, line) :: acc
              else Token(TokenType.Greater, line) :: acc

            loop(current + 1, line, nextAcc)
          case Grammar.Slash =>
            val nextAcc = if (matchNext(current, Grammar.Slash)) {
              skipUntilEndLine(current, line)
              acc
            } else {
              Token(TokenType.Slash, line) :: acc
            }
            loop(current + 1, line, nextAcc)
          case Grammar.WhiteSpace     => loop(current + 1, line, acc)
          case Grammar.NextLine       => loop(current + 1, line + 1, acc)
          case Grammar.CarriageReturn => loop(current + 1, line + 1, acc)
          case Grammar.Tab            => loop(current + 1, line, acc)
          case Grammar.Quote =>
            val result  = string(current + 1, line)
            val nextAcc = Token(TokenType.String, Some(s"\"${result.token}\""), Some(result.token), result.line) :: acc
            loop(result.current + 1, line, nextAcc)
          case c if Grammar.isDigit(c) =>
            val result = number(current, line)
            val nextAcc = Token(
              TokenType.Number,
              Some(result.token),
              Some(java.lang.Double.parseDouble(result.token)),
              result.line
            ) :: acc
            loop(result.current + 1, line, nextAcc)
          case c if Grammar.isAlpha(c) =>
            val result    = identifier(current, line)
            val tokenType = Grammar.Keywords.get(result.token).fold[TokenType](TokenType.Identifier)(identity)
            val nextAcc   = Token(tokenType, Some(result.token), Some(result.token), result.line) :: acc
            loop(result.current, line, nextAcc)
          case c =>
            Errors.error(line, s"Unexpected character: $c")
            throw new RuntimeException(s"Unexpected character: $c at line: $line")
        }
      }
    }
    loop(0, 1, List.empty).reverse
  }

  private def identifier(current: Int, line: Int): Result = {
    def loop(cur: Int, acc: StringBuilder): Result = {
      if (Grammar.isAlphaNumeric(peek(cur))) {
        loop(cur + 1, acc.append(next(cur)))
      } else {
        Result(cur, line, acc.toString())
      }
    }

    loop(current, new StringBuilder)
  }

  private def number(current: Int, line: Int): Result = {
    def loop(cur: Int, acc: StringBuilder): Result = {
      if (nonEnd(cur) && Grammar.isDigit(peek(cur))) {
        val c = next(cur)
        loop(cur + 1, acc.append(c))
      } else if (nonEnd(cur) && peek(cur) == Grammar.Dot) {
        loop(cur + 1, acc)
      } else {
        Result(cur, line, acc.toString())
      }
    }
    loop(current, new StringBuilder)
  }

  private def string(current: Int, line: Int): Result = {
    def loop(cur: Int, l: Int, acc: StringBuilder): Result = {
      if (nonEnd(cur) && peek(current) != Grammar.Quote) {
        val c = next(cur)
        if (c == Grammar.NextLine) {
          loop(cur + 1, l + 1, acc.append(c))
        } else {
          loop(cur + 1, l, acc.append(c))
        }
      } else Result(cur + 1, line, acc.toString())
    }

    val result    = loop(current, line, new StringBuilder)
    val nextToken = next(result.current)
    if (nextToken != Grammar.Quote) {
      Errors.error(result.line, s"The string must be enclosed in double quotes.")
      throw new RuntimeException("The string must be enclosed in double quotes.")
    }
    Result(result.current + 1, result.line, result.token)
  }

  private def skipUntilEndLine(current: Int, line: Int): Result = {
    def loop(cl: Int): Result = {
      val token = next(cl)
      token match {
        case Grammar.NextLine => Result(cl + 1, line + 1, null)
        case _                => loop(cl + 1)
      }
    }

    loop(current)
  }

  private def next(current: Int): Char = {
    source.charAt(current)
  }

  private def peek(current: Int): Char = {
    if (isEnd(current)) '\u0000'
    else source.charAt(current)
  }

  private def peekNext(current: Int): Char = {
    if (isEnd(current + 1)) '\u0000'
    else source.charAt(current + 1)
  }
  private def isEnd(current: Int): Boolean = {
    current >= source.length
  }

  private def nonEnd(current: Int): Boolean = {
    !isEnd(current)
  }

  private def matchNext(current: Int, expected: Char): Boolean = {
    if (isEnd(current + 1)) false
    else peekNext(current) == expected
  }

//  def handleComments(current: Int)
}

object Scanner {

  private case class Result(current: Int, line: Int, token: String)

  def apply(source: String): Scanner = {
    new Scanner(source)
  }
}
