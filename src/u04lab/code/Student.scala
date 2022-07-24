package u04lab.code

import Lists.List
import Lists.List._
import org.junit.jupiter.api.Assertions.fail
import u04lab.code.Lists.List.{Cons, Nil, append, contains, map} // import custom List type (not the one in Scala stdlib)

trait Student {
  def name: String
  def year: Int
  def enrolling(course: Course*): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?
}

trait Course {
  def name: String
  def teacher: String
}

object Student {
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)

  private case class StudentImpl(override val name: String,
                                 override val year: Int) extends Student {

    private var _courses: List[Course] = Nil()

    override def enrolling(courseToAdd: Course*): Unit = courseToAdd foreach (course => _courses = append(_courses, Cons(course, Nil())))

    override def courses: List[String] = map(_courses)(_.name)

    override def hasTeacher(teacher: String): Boolean = contains(map(_courses)(_.teacher))(teacher)
  }
}

object SameTeacher {
  def unapply(courses: List[Course]): Option[String] = courses match {
    case Cons(head, tail) if (foldLeft(tail)(true)((acc,l) => acc && head.teacher == l.teacher)) => Some(head.teacher)
    case _ => None
  }
}

object StudentCourseTest {
  import org.junit.jupiter.api.Assertions.{assertEquals, assertTrue}
  import org.junit.jupiter.api.{BeforeEach, Test}

  val cPPS: Course = Course("PPS","Viroli")
  val cOOP: Course = Course("OOP","Viroli")
  val cPCD: Course = Course("PCD","Ricci")
  val cSDR: Course = Course("SDR","D'Angelo")
  val cASW: Course = Course("AWS", "Mirri")
  val s1: Student = Student("mattia",2015)

  @BeforeEach
  def initCourses(): Unit = {
    s1.enrolling(cPPS)
    s1.enrolling(cPCD)
  }

  @Test def testCourses(): Unit = {
    assertEquals(Cons("PPS",Cons("PCD", Nil())), s1.courses)
  }

  @Test def testHasTeacher(): Unit = {
    assertTrue(s1.hasTeacher("Viroli"))
    assertTrue(s1.hasTeacher("Ricci"))
  }

  @Test def testMultipleEnrolling(): Unit = {
    s1.enrolling(cSDR, cASW)
    assertEquals(Cons("PPS",Cons("PCD", Cons("SDR", Cons("AWS", Nil())))), s1.courses)
    assertTrue(s1.hasTeacher("D'Angelo"))
    assertTrue(s1.hasTeacher("Mirri"))
  }

  @Test def testUnapplyMatches(): Unit = {
    List(cPPS, cOOP) match {
      case SameTeacher("Viroli") => assertTrue(true)
      case _ => fail("Should have matched with sameTeacher")
    }
  }

  @Test def testUnapplyDoesNotMatch(): Unit = {
    List(cPPS, cOOP, cPCD) match {
      case SameTeacher(_) => fail("Should not match with sameTeacher")
      case _ => assertTrue(true)
    }
  }
}

object Course {
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)

  private case class CourseImpl(override val name: String,
                                override val teacher: String) extends Course
}

/*
object Try extends App {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017
  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)
  println(s1.courses, s2.courses, s3.courses) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true
}*/

/** Hints:
 * - simply implement Course, e.g. with a case class
 * - implement Student with a StudentImpl keeping a private Set of courses
 * - try to implement in StudentImpl method courses with map
 * - try to implement in StudentImpl method hasTeacher with map and find
 * - check that the two println above work correctly
 * - refactor the code so that method enrolling accepts a variable argument Course*
 */