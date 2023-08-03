import java.nio.charset.Charset
import java.security.cert.X509CertSelector
import scala.util.Random


val xs =   "abcdefghijklmnopqrstuvwxyz".toList

xs.foreach { c =>
  println(s"""private val ${c.toUpper}: Char = '$c'""")
}

xs.foldLeft("Set(") {
  case (acc, c) => acc + s"${c.toUpper},"
}


val x = "abcd"

x.foldLeft("") { case (acc, l) => l + acc}
x.foldRight("") { case (l, acc) => l + acc}