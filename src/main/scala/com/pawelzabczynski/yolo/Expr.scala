package com.pawelzabczynski.yolo

sealed trait Expr extends AstTag

final case class Assign(name: Token, value: Expr)                        extends Expr
final case class Binary(left: Expr, operator: Token, right: Expr)        extends Expr
final case class Call(callee: Expr, paren: Token, arguments: List[Expr]) extends Expr
final case class Get(obj: Expr, name: Token)                             extends Expr
final case class Grouping(expression: Expr)                              extends Expr
final case class Literal(value: Value)                                   extends Expr
final case class Logical(left: Expr, operator: Token, right: Expr)       extends Expr
final case class Set(obj: Expr, name: Token, value: Expr)                extends Expr
final case class Super(keyword: Token, method: Token)                    extends Expr
final case class This(keyword: Token)                                    extends Expr
final case class Unary(operator: Token, right: Expr)                     extends Expr
final case class Variable(name: Token)                                   extends Expr

