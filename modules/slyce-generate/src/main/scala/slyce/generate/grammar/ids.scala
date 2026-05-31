package slyce.generate.grammar

import java.util.UUID

opaque type AnonListNtId = UUID
object AnonListNtId {
  def random: AnonListNtId = UUID.randomUUID
}
