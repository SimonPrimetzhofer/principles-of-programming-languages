package persons

enum Study {
  case INF, WIWI, WINF, LAW
}

enum Science {
  case Computer, Economy, SoftwareEng, Electronics
}

class Person(val firstName: String, val lastName: String) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Person]

  override def equals(other: Any): Boolean = other match {
    case that: Person =>
      (that canEqual this) &&
        firstName == that.firstName &&
        lastName == that.lastName
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(lastName)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class Student(firstName: String, lastName: String, val study: Study) extends Person(firstName, lastName) {
  override def toString = s"$firstName $lastName, studying $study"
}

class Professor(firstName: String, lastName: String, val research: Science) extends Person(firstName, lastName) {
  override def toString = s"$firstName $lastName, working in $research"
}
