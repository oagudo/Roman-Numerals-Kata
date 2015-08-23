package model

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class RomanNumeralsTest extends FunSuite with Checkers {

  private case class NumberRep(val roman: String, val digits: Int)

  private val numbers : Array[NumberRep] = Array(
    (NumberRep("I", 1)),
    (NumberRep("II", 2)),
    (NumberRep("III", 3)),
    (NumberRep("IV", 4)),
    (NumberRep("V", 5)),
    (NumberRep("VII", 7)),
    (NumberRep("IX", 9)),
    (NumberRep("X", 10)),
    (NumberRep("XI", 11)),
    (NumberRep("XXX", 30)),
    (NumberRep("LVIII", 58)),
    (NumberRep("MCMXC", 1990)),
    (NumberRep("MCMXCIX", 1999)),
    (NumberRep("MMCLXXIX", 2179)),
    (NumberRep("MMDCCXVIII", 2718)))

  test("Roman numbers can be converted to its arabic number representation") {
    for (n <- numbers) {
      check (RomanNumerals.romanize(n.digits).equals(n.roman))
    }
  }

  test("Arabic numbers can be converted to its roman number representation") {
    for (n <- numbers) {
      check (RomanNumerals.numerize(n.roman).equals(n.digits))
    }
  }
}

