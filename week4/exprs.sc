
package week4

trait Expr
case class Number(n:Int) extends Expr
case class Sum(x:Expr,y:Expr) extends Expr

object exprs{
  def show(x:Expr): String = x match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + "+" + show(r)
  }
}
