package coll

import persons.{Person, Professor, Science, Student, Study}

object FSetApp extends App {
   var students: FSet[Student] = FSet()
    students = students.add(new Student("Hans", "Maier", Study.INF))
    students = students.add(new Student("Franz", "Berger", Study.WIWI))
    students = students.add(new Student("Alois", "Berger", Study.LAW))
    students.foreach(println(_))

    val persons: FSet[Person] = students.add(new Professor("Niklas", "Maier", Science.Computer))
    persons.foreach(println(_))

    HashTree.printTree(persons.tree)

    println(persons.map(student => student.lastName).sum(lastname => lastname.length))

    println(students.filter(student => student.study == Study.INF)
      .toString("SEP ", "INF ", " INF done", student => student.firstName + " " + student.lastName))

    println(persons.fold("firstnames: ")((concat, person) => concat + person.firstName))

    println(persons.sum(person => person.lastName.length))

    println(persons.count)

    println(persons.toString(", ", "persons ", " END", person => person.lastName))
}
