package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "in singleton set")
      assert(!contains(s1, 2), "not in singleton set")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains all elements that are in both set") {
    new TestSets {
      val t = intersect(union(s1, s3), union(s2, s3))
      assert(contains(t, 3), "Intersect 3")
      assert(!contains(t, 2), "Intersect 1")
      assert(!contains(t, 1), "Intersect 3")
    }
  }

  test("diff returns a set which contains all the elements of the set s that are not in the set t") {
    new TestSets {
      val s = union(union(s1, s2), union(s3, s4))
      val t = union(union(s3, s4), union(s5, s6))
      val r = diff(s, t)
      assert(contains(r, 1), "Diff 1")
      assert(contains(r, 2), "Diff 2")
      assert(!contains(r, 3), "Diff 3")
      assert(!contains(r, 4), "Diff 4")
      assert(!contains(r, 5), "Diff 5")
      assert(!contains(r, 7), "Diff 7")
    }
  }

  test("returns the subset of s for which p holds") {
    new TestSets {
      val s = union(union(s1, s2), union(s3, s4))
      def p(x: Int): Boolean = x >= 3
      assert(!filter(s, p)(1), "Filter 1")
      assert(!filter(s, p)(2), "Filter 2")
      assert(filter(s, p)(3), "Filter 3")
      assert(filter(s, p)(4), "Filter 4")
      assert(!filter(s, p)(5), "Filter 5")
    }
  }

  test("forall") {
    new TestSets {
      val s = union(union(s1, s2), union(s3, s4))
      val t = union(union(s0, s2), union(s3, s4))
      def p(x: Int): Boolean = x > 0
      assert(forall(s, p), "forall s")
      assert(!forall(t, p), "forall t")
    }
  }

  test("exists") {
    new TestSets {
      val s = union(union(s1, s2), union(s3, s4))
      val t = union(union(s0, s2), union(s3, s4))
      def p(x: Int): Boolean = x == 0
      assert(exists(t, p), "exits t")
      assert(!exists(s, p), "exits s")
    }
  }

  test("map") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      def m(x: Int): Int = x * 2
      val r = map(s, m)
      assert(contains(r, 2), "map 2")
      assert(contains(r, 4), "map 4")
      assert(contains(r, 6), "map 6")
      assert(!contains(r, 1), "map 1")
      assert(!contains(r, 3), "map 3")
    }
  }
}
