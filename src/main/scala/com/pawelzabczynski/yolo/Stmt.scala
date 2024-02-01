package com.pawelzabczynski.yolo

sealed trait Stmt extends AstTag

final case class Block(stmts: List[Stmt])                                                  extends Stmt
final case class Class(name: Token, superClass: Option[Variable], methods: List[Function]) extends Stmt
final case class Expression(expression: Expr)                                              extends Stmt
final case class Function(name: Token, params: List[Token], body: List[Stmt])              extends Stmt
final case class If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])           extends Stmt
final case class Print(expression: Expr)                                                   extends Stmt
final case class Return(keyword: Token, value: Option[Expr])                               extends Stmt
final case class Var(name: Token, initializer: Option[Expr])                               extends Stmt
final case class While(condition: Expr, body: Stmt)                                        extends Stmt
