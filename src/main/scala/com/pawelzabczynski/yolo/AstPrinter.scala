package com.pawelzabczynski.yolo

object AstPrinter {
  def print(e: AstTag): String = {
    def loop(e: AstTag, acc: StringBuilder): StringBuilder = {
      e match {
        case Assign(name, value) => acc.append(parenthesize2("=", name, value))
        case Binary(left, operator, right) =>
          acc.append(parenthesize(operator.lexeme.getOrElse("missing operator"), left, right))
        case Call(callee, _, arguments) => acc.append(parenthesize2("call", callee :: arguments: _*))
        case Get(obj, name)             => acc.append(parenthesize2(".", obj, name))
        case Grouping(expression)       => acc.append(parenthesize("group", expression))
        case Literal(value)             => acc.append(value)
        case Logical(left, operator, right) =>
          acc.append(parenthesize(operator.lexeme.getOrElse("missing operator"), left, right))
        case Set(obj, name, value)  => acc.append(parenthesize("=", obj, name, value))
        case Super(_, method)       => acc.append(parenthesize2("super", method))
        case This(_)                => acc.append("this")
        case Unary(operator, right) => acc.append(operator.lexeme.getOrElse("missing unary operator"), right)
        case Variable(name)         => acc.append(name.lexeme.getOrElse("missing variable"))
        case Block(stmts)           => acc.append(transformStmts("block", stmts: _*))
        case Class(name, superClass, methods) =>
          acc.append(s"(class ${name.lexeme.getOrElse("missing-class-name")}")
          acc.append(superClass.map(v => s" < ${v.name} ").getOrElse(""))
          acc.append(methods.map(print))
        case Expression(expression) => acc.append(parenthesize(";", expression))
        case Function(name, params, body) =>
          acc.append(s"(fun ${name.lexeme.getOrElse("missing-function-name")}(")
          acc.append(params.map(_.lexeme.getOrElse("missing-arg-name")).mkString(", "))
          acc.append(")")
          acc.append(body.map(print))
          acc.append(")")
        case If(condition, thenBranch, maybeElseBranch) =>
          maybeElseBranch match {
            case Some(elseBranch) => acc.append(parenthesize2("if", condition, thenBranch, elseBranch))
            case None             => acc.append(parenthesize2("if", thenBranch))
          }
        case Print(expression) => acc.append(parenthesize("print", expression))
        case Return(_, value) =>
          value match {
            case Some(v) => acc.append(parenthesize("return", v))
            case None    => acc.append("(return)")
          }
        case Var(name, initializer) =>
          initializer match {
            case Some(init) =>
              acc.append(parenthesize2("var", name, Token(TokenType.Identifier, Some("="), None, -1), init))
            case None => acc.append(parenthesize2("var", name))
          }
        case While(condition, body) =>
          acc.append(parenthesize2("while", condition, body))
        case _ => acc
      }
    }

    def transformStmts(name: String, statements: Stmt*): StringBuilder = {
      val sb = new StringBuilder()
      sb.append("( ")
        .append(name)
        .append(" ")
      statements.foreach { s =>
        loop(s, sb)
      }
      sb.append(")")
    }

    def parenthesize2(name: String, expr: AstTag*): StringBuilder = {
      val sb = new StringBuilder()
      sb.append("(")
      sb.append(name)
      transform(expr: _*)
      sb.append(")")
    }

    def parenthesize(name: String, expr: AstTag*): StringBuilder = {
      val sb = new StringBuilder()
      sb.append("(")
      sb.append(name)

      expr.foreach { v =>
        sb.append(" ")
        sb.append(loop(v, new StringBuilder()))
      }

      sb.append(")")
    }

    def transform(objs: AstTag*): StringBuilder = {
      objs.foldLeft(new StringBuilder()) {
        case (acc, expr: Expr)   => acc.append(loop(expr, new StringBuilder()))
        case (acc, token: Token) => acc.append(token.lexeme.getOrElse("missing"))
        case (acc, _)            => acc
      }
    }

    loop(e, new StringBuilder()).result()
  }
}
