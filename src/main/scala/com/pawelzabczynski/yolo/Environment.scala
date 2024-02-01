package com.pawelzabczynski.yolo

import scala.collection.mutable.{Map => MMap}

final case class Environment(enclosing: Option[Environment], values: MMap[String, Value]) {

  def get(name: Token): Value = {
    name.lexeme match {
      case Some(lexeme) =>
        values.get(lexeme) match {
          case Some(value) => value
          case None =>
            enclosing match {
              case Some(enc) => enc.get(name)
              case None      => throw new RuntimeException(s"Undefined variable $name")
            }
        }
      case None => throw new RuntimeException(s"Undefined variable $name")
    }
  }

  def assign(name: Token, value: Value): Unit = {
    name.lexeme match {
      case Some(lexeme) =>
        values.get(lexeme) match {
          case Some(_) => values.put(lexeme, value)
          case None =>
            enclosing match {
              case Some(enc) => enc.assign(name, value)
              case None      => throw new RuntimeException(s"Undefined variable $name")
            }
        }
      case None => throw new RuntimeException(s"Undefined variable $name")
    }
  }

  def define(name: String, value: Value): Unit = {
    values.put(name, value)
  }

  def ancestor(distance: Int): Environment = {
    def loop(cnt: Int, current: Environment): Environment = {
      if (cnt == distance) current
      else {
        enclosing match {
          case Some(enc) => loop(cnt + 1, enc)
          case None      => throw new RuntimeException(s"Missing environment, failed at $cnt, try reach $distance")
        }
      }
    }

    loop(1, this)
  }

  def getAt(distance: Int, name: String): Value = {
    ancestor(distance).values(name)
  }

  def assignAt(distance: Int, name: Token, value: Value): Unit = {
    ancestor(distance).assign(name, value)
  }

  override def toString: String = {
    enclosing match {
      case Some(enc) => s"$values -> $enc"
      case None      => s"$values"
    }
  }
}

object Environment {
  val empty: Environment = Environment(None, MMap.empty)
}
