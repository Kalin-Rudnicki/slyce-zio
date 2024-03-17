package slyce.test.json

object Types {

  final case class Person(
      firstName: String,
      lastName: String,
      age: Option[Int],
      friends: List[Friend],
  )
  object Person {

    implicit val decoder: JsonDecoder[Person] =
      JsonDecoder.jsonObject.flatMap { _ =>
        {
          JsonDecoder.forKey[String]("firstName") <*>
            JsonDecoder.forKey[String]("lastName") <*>
            JsonDecoder.forKey[Option[Int]]("age") <*>
            JsonDecoder.forKey[List[Friend]]("friends")
        }.map(Person.apply)
      }

  }

  final case class Friend(
      name: String,
      friendsSinceYear: Int,
  )
  object Friend {

    implicit val decoder: JsonDecoder[Friend] =
      JsonDecoder.jsonObject.flatMap { _ =>
        {
          JsonDecoder.forKey[String]("name") <*>
            JsonDecoder.forKey[Int]("friendsSinceYear")
        }.map(Friend.apply)
      }

  }

}
