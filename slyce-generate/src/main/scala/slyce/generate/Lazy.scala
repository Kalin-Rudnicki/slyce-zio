package slyce.generate

final class Lazy[+T](t: => T) {

  lazy val value: T = t

  override def toString: String =
    s"Lazy($value)"

}

object Lazy {

  def selfMap[I, K, V](input: List[I])(elemF: (I, K => Lazy[V]) => (K, V)): Map[K, V] = {
    lazy val lazyMap: Map[K, V] = {
      def ef(k: K): Lazy[V] = Lazy(lazyMap(k))
      input.map(elemF(_, ef)).toMap
    }

    lazyMap
  }

}
