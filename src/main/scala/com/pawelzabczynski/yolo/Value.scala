package com.pawelzabczynski.yolo

sealed trait Value extends AstTag
final case class NumericVal(value: Double) extends Value {
  override def toString: String = {
    value.toString
  }
}
final case class StringVal(value: String) extends Value {
  override def toString: String = {
    value
  }
}
case object NilVal extends Value {
  override def toString: String = {
    "nil"
  }
}
