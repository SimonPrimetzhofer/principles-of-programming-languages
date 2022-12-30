package coll

import persons.{Person, Professor, Science, Student, Study}

object FListApp extends App {
  var students: FList[Student] = FNil
  students = students.add(new Student("Hans", "Maier", Study.INF))
  students = students.add(new Student("Franz", "Berger", Study.WIWI))
  students.foreach(println(_))

  val persons : FList[Person] = students.add(new Professor("Niklas", "Wirth", Science.Computer))
  persons.foreach(println(_))

  println(persons.map(student => student.lastName).sum(lastname => lastname.length))

  println(students.filter(student => student.study == Study.INF)
    .toString("SEP ", "INF ", " INF done", student => student.firstName + " " + student.lastName))

  println(persons.fold("firstnames: ")((concat, person) => concat + person.firstName))

  println(persons.sum(person => person.lastName.length))

  println(persons.count)

  println(persons.toString(", ", "persons ", " END", person => person.lastName))

}