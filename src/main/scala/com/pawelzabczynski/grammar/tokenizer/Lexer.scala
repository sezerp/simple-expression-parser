package com.pawelzabczynski.grammar.tokenizer

import com.pawelzabczynski.grammar.tokenizer.TokenizerError.InvalidTokenError

import scala.annotation.tailrec

object Lexer {
  private val NL: Char          = '\n'
  private val OP: Char          = '('
  private val CP: Char          = ')'
  private val WHITE_SPACE: Char = ' '
  private val COLON: Char       = ':'
  private val TAB: Char         = '\t'

  private val ZERO: Char  = '0'
  private val ONE: Char   = '1'
  private val TWO: Char   = '2'
  private val THREE: Char = '3'
  private val FOUR: Char  = '4'
  private val FIVE: Char  = '5'
  private val SIX: Char   = '6'
  private val SEVEN: Char = '7'
  private val EIGHT: Char = '8'
  private val NINE: Char  = '9'

  private val A: Char = 'a'
  private val B: Char = 'b'
  private val C: Char = 'c'
  private val D: Char = 'd'
  private val E: Char = 'e'
  private val F: Char = 'f'
  private val G: Char = 'g'
  private val H: Char = 'h'
  private val I: Char = 'i'
  private val J: Char = 'j'
  private val K: Char = 'k'
  private val L: Char = 'l'
  private val M: Char = 'm'
  private val N: Char = 'n'
  private val O: Char = 'o'
  private val P: Char = 'p'
  private val Q: Char = 'q'
  private val R: Char = 'r'
  private val S: Char = 's'
  private val T: Char = 't'
  private val U: Char = 'u'
  private val V: Char = 'v'
  private val W: Char = 'w'
  private val X: Char = 'x'
  private val Y: Char = 'y'
  private val Z: Char = 'z'

  private val PLUS: Char   = '+'
  private val MINUS: Char  = '-'
  private val DIVIDE: Char = '/'
  private val MUL: Char    = '*'
  private val POWER: Char  = '*'
  private val GREATER: Char = '>'
  private val EQUAL: Char = '='
  private val SMALLER: Char = '<'

  private val CHARACTERS: Set[Char] = Set(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)
  private val NUMBERS: Set[Char]    = Set(ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE)
  private val OPERATORS: Set[Char]  = Set(PLUS, MINUS, DIVIDE, MUL, POWER, GREATER, EQUAL, SMALLER)
  private val SPECIAL_CHARACTERS: Set[Char] = Set(NL, OP, CP, WHITE_SPACE, COLON, TAB)
  private val TERMINATION_TOKENS: Set[Char] = Set(WHITE_SPACE, COLON, TAB, OP, NL, CP)

  private val ALLOWED_TOKENS: Set[Char] = CHARACTERS ++ NUMBERS ++ OPERATORS ++ SPECIAL_CHARACTERS

  def lex(expression: String): Either[TokenizerError, List[Lexeme]] = {
    def isEnd(numberTokens: Int): Int => Boolean = { position =>
      numberTokens <= position
    }
    def hasNext(numberTokens: Int): Int => Boolean = { position =>
      position + 1 < numberTokens
    }
    val end  = isEnd(expression.length)
    val next = hasNext(expression.length)

    @tailrec
    def loop(position: Int, line: Int, column: Int, acc: List[Lexeme]): Either[TokenizerError, List[Lexeme]] = {
      if (end(position)) {
        Right(acc)
      } else {
        val token = expression(position)
        if (isAllowedToken(token)) {
          if (token == NL) {
            loop(position + 1, line + 1, column, acc)
          } else if (token == WHITE_SPACE) {
            loop(position + 1, line, column, acc)
          } else {
            collectLexeme(position, line, column, expression, next) match {
              case Right((pos, l, c, lex)) => loop(pos, l, c, lex :: acc)
              case Left(err)               => Left(err)
            }

          }
        } else {
          Left(InvalidTokenError(line, column, token))
        }
      }
    }

    loop(0, 1, 1, List.empty)
  }

  private def collectLexeme(
      pos: Int,
      line: Int,
      column: Int,
      expression: String,
      hasNext: Int => Boolean
  ): Either[TokenizerError, (Int, Int, Int, Lexeme)] = {
    @tailrec
    def loop(cnt: Int, l: Int, col: Int, acc: List[Char]): Either[TokenizerError, (Int, Int, Int, Lexeme)] = {
      if (hasNext(cnt)) {
        val token     = expression(cnt)
        val nextToken = expression(cnt + 1)
        if (ALLOWED_TOKENS.contains(token)) {
          if (TERMINATION_TOKENS.contains(nextToken)) {
            if (token == NL) {
              val lex = acc.foldRight(new StringBuilder) { case (c, acc) => acc.append(c) }.toString()
              Right((cnt + 1, l, col, Lexeme(lex, l, col)))
            } else if (TERMINATION_TOKENS.contains(token)) {
              Right((cnt + 1, l, col, Lexeme(token.toString, l, col)))
            } else {
              val lex = (token :: acc).foldRight(new StringBuilder) { case (c, acc) => acc.append(c) }.toString()
              Right((cnt + 1, l, col, Lexeme(lex, l, col)))
            }
          } else if (TERMINATION_TOKENS.contains(token)) {
            Right((cnt + 1, l, col, Lexeme(token.toString, l, col)))
          } else {
            loop(cnt + 1, l, col + 1, token :: acc)
          }
        } else {
          Left(InvalidTokenError(l, col, token))
        }
      } else {
        val lex = acc.foldRight(new StringBuilder) { case (c, acc) => acc.append(c) }.toString()
        Right((cnt + 1, l, col, Lexeme(lex, l, col)))
      }
    }

    loop(pos, line, column, List.empty)
  }
  private def isAllowedToken(token: Char): Boolean = {
    ALLOWED_TOKENS.contains(token)
  }

}
