package coll

import persons.{Person, Professor, Science, Student, Study}

object FListApp extends App {
  var students: FList[Student] = FNil
  students = students.add(new Student("Hans", "Maier", Study.INF))
  students = students.add(new Student("Franz", "Berger", Study.WIWI))
  students.foreach(println(_))

  val persons : FList[Person] = students.add(new Professor("Niklas", "Wirth", Science.Computer))
  persons.foreach(println(_))

}