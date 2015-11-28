package Interpreter

import java.io.File
import scala.annotation.tailrec
import scala.util.matching.Regex
sealed trait Expression {
  def evaluate(context: String): Seq[File] = {
    val d = new File(context)
    if (d.exists && d.isDirectory) {
      @tailrec
      def listFiles(files: List[File], result: List[File]): List[File] = files match {
        case Nil => result
        case head :: tail if head.isDirectory =>
          listFiles(Option(head.listFiles).map(_.toList ::: tail).getOrElse(tail), result)
        case head :: tail if head.isFile =>
          listFiles(tail, head :: result)
      }
      listFiles(List(d), Nil).reverse
    } else {
      List[File]()
    }
  }
}
case class All() extends Expression
case class FileName(pattern: Regex) extends Expression {
  override def evaluate(dir: String): Seq[File] = {
    val results = super.evaluate(dir)
    results.filter(_.getName match {
      case pattern() => true
      case _ => false
    })
  }
}
case class Bigger(size: Int) extends Expression {
  override def evaluate(dir: String): Seq[File] = {
    val results = super.evaluate(dir)
    results.filter(_.length > size)
  }
}
case class Smaller(size: Int) extends Expression {
  override def evaluate(dir: String): Seq[File] = {
    val results = super.evaluate(dir)
    results.filter(_.length < size)
  }
}
case class Writable() extends Expression {
  override def evaluate(dir: String): Seq[File] = {
    val results = super.evaluate(dir)
    results.filter(_.canWrite)
  }
}
case class Readable() extends Expression {
  override def evaluate(dir: String): Seq[File] = {
    val results = super.evaluate(dir)
    results.filter(_.canRead)
  }
}
case class Not(expr: Expression) extends Expression {
  override def evaluate(dir: String): Seq[File] =
    super.evaluate(dir) diff expr.evaluate(dir)
}
case class And(lhs: Expression, rhs: Expression) extends Expression {
  override def evaluate(dir: String): Seq[File] =
    lhs.evaluate(dir) intersect rhs.evaluate(dir)
}
case class Or(lhs: Expression, rhs: Expression) extends Expression {
  override def evaluate(dir: String): Seq[File] =
    lhs.evaluate(dir) union rhs.evaluate(dir)
}
