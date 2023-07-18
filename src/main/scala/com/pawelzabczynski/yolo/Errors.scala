package com.pawelzabczynski.yolo

object Errors {
  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println(s"[line: $line] Error $where + $message")
  }
}
