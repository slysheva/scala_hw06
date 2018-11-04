package fintech.homework06
import scala.collection.mutable
/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

object  EqSyntax{
  implicit class EqOps[A: Eq](val left: A) {
    def ====(right: A): Boolean = implicitly[Eq[A]].equiv(left, right)
  }
}

trait EqInstances{
  import EqSyntax._

  implicit val stringEq: Eq[String] = (l: String, r: String) => l == r

  implicit val intEq: Eq[Int] = (l: Int, r: Int) => l == r

  implicit def seqEq[A: Eq]: Eq[Seq[A]] = (lft: Seq[A], rgt: Seq[A]) => {
    if (lft.size != rgt.size)
      false
    else {
      val zipped = lft.zip(rgt)
      for (pair <- zipped) {
        if (!(pair._1 ==== pair._2))
          false
      }
      true
    }
  }

  implicit def optionEq[A: Eq]: Eq[Option[A]] = (lft: Option[A], rgt: Option[A]) => {
    (lft, rgt) match {
      case (None, None) => true
      case (Some(a), Some(b)) => a ==== b
      case _ => false
    }
  }

  implicit def mapEq[K: Eq, V: Eq]: Eq[Map[K, V]] = (lft: Map[K, V], rgt: Map[K, V]) => {
    if (lft.size != rgt.size)
      false
    else {
      var used = mutable.Set[K]()
      var ans = true
      for (left <- lft) {
        var foundEqual = false
        for (right <- rgt) {
          if (!foundEqual && left._1 ==== right._1 && left._2 ==== right._2 && !(used contains right._1)) {
            used += right._1
            foundEqual = true
          }
        }
        ans = ans && foundEqual
      }
      ans
    }
  }
}

object Eq extends EqInstances
