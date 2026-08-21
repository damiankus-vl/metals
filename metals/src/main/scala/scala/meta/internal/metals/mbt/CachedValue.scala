package scala.meta.internal.metals.mbt

import java.util.concurrent.atomic.AtomicReference

import scala.meta.internal.metals.mbt.CacheSlot.Computed
import scala.meta.internal.metals.mbt.CacheSlot.NotComputed

/**
 * Whether a [[CachedValue]] has been computed.
 *
 * `Computed(Map.empty)` is a real result. An empty map used as a sentinel could
 * not be told apart from [[NotComputed]].
 */
private[mbt] sealed trait CacheSlot[+A]
private[mbt] object CacheSlot {
  case object NotComputed extends CacheSlot[Nothing]
  final case class Computed[A](value: A) extends CacheSlot[A]
}

/** A value computed on first read and kept until [[CachedValue.clear]]. */
private[mbt] final class CachedValue[A] {
  private val slot = new AtomicReference[CacheSlot[A]](NotComputed)

  def getOrCompute(compute: () => A): A =
    slot.get() match {
      case Computed(value) => value
      case NotComputed =>
        val computed = compute()
        slot.compareAndSet(NotComputed, Computed(computed))
        // `compareAndSet` fails two ways. Another thread computed first, and
        // its value is read instead. Or `clear` ran, and the value computed
        // here is returned without being stored.
        getOrElse(computed)
    }

  /** The value if it was computed, otherwise `fallback`. Stores nothing. */
  def getOrElse(fallback: => A): A =
    slot.get() match {
      case Computed(value) => value
      case NotComputed => fallback
    }

  def clear(): Unit = slot.set(NotComputed)
}
