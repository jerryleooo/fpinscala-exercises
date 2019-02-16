sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new Exception("empty list")
      case Cons(_, t) => t
    }
  } 

  def setHead[A](l: List[A], head: A): List[A] = {
    l match {
      case Nil => throw new Exception("empty list")
      case Cons(_, t) => Cons(head, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => {
        if(n > 0)
          throw new Exception("empty list")
        else
          Nil
      }
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if(f(h)) dropWhile(t, f) else Cons(h, t)
    }
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmtpy) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, h) => acc + 1)
  }
}
