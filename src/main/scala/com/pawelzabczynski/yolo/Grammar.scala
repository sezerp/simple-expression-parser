package com.pawelzabczynski.yolo

object Grammar {
  case class CharacterDescription(value: Char, `type`: TokenType)

  // special characters
  val LeftParen: Char      = '('
  val RightParen: Char     = ')'
  val LeftBrace: Char      = '{'
  val RightBrace: Char     = '}'
  val Comma: Char          = ','
  val Dot: Char            = '.'
  val Minus: Char          = '-'
  val Plus: Char           = '+'
  val Semicolon: Char      = ';'
  val Star: Char           = '*'
  val Bang: Char           = '!'
  val Equal: Char          = '='
  val Less: Char           = '<'
  val Greater: Char        = '>'
  val Slash: Char          = '/'
  val Tab: Char            = '\t'
  val CarriageReturn: Char = '\r'
  val NextLine: Char       = '\n'
  val WhiteSpace: Char     = ' '
  val Quote: Char          = '"'
  val Underscore: Char     = '_'
// numbers
  val Zero: Char  = '0'
  val One: Char   = '1'
  val Two: Char   = '2'
  val Three: Char = '3'
  val Four: Char  = '4'
  val Five: Char  = '5'
  val Six: Char   = '6'
  val Seven: Char = '7'
  val Eight: Char = '8'
  val Nine: Char  = '9'
// alpha
  val A: Char = 'a'
  val B: Char = 'b'
  val C: Char = 'c'
  val D: Char = 'd'
  val E: Char = 'e'
  val F: Char = 'f'
  val G: Char = 'g'
  val H: Char = 'h'
  val I: Char = 'i'
  val J: Char = 'j'
  val K: Char = 'k'
  val L: Char = 'l'
  val M: Char = 'm'
  val N: Char = 'n'
  val O: Char = 'o'
  val P: Char = 'p'
  val Q: Char = 'q'
  val R: Char = 'r'
  val S: Char = 's'
  val T: Char = 't'
  val U: Char = 'u'
  val V: Char = 'v'
  val W: Char = 'w'
  val X: Char = 'x'
  val Y: Char = 'y'
  val Z: Char = 'z'

  // keywords
  val Keywords: Map[String, TokenType] = Map(
    "and"    -> TokenType.And,
    "class"  -> TokenType.Class,
    "else"   -> TokenType.Else,
    "false"  -> TokenType.False,
    "for"    -> TokenType.For,
    "fun"    -> TokenType.Fun,
    "if"     -> TokenType.If,
    "nil"    -> TokenType.Nil,
    "or"     -> TokenType.Or,
    "print"  -> TokenType.Print,
    "return" -> TokenType.Return,
    "super"  -> TokenType.Super,
    "this"   -> TokenType.This,
    "true"   -> TokenType.True,
    "var"    -> TokenType.Var,
    "while"  -> TokenType.While
  )

//  character sets
  val Numbers: Set[Char]    = Set(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine)
  val Characters: Set[Char] = Set(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

  def isDigit(c: Char): Boolean = {
    Numbers.contains(c)
  }

  def isAlpha(c: Char): Boolean = {
    Characters.contains(c.toLower)
  }

  def isAlphaNumeric(c: Char): Boolean = {
    isAlpha(c) || isDigit(c)
  }

}
