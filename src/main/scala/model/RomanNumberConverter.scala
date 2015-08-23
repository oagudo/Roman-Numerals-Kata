package model

import java.security.InvalidParameterException
import scala.math._

trait RomanNumberConverter {
  def romanize(n: Int) : String
  def numerize(n: String) : Int
}

object RomanConverter extends RomanNumberConverter {
  
  override def romanize(n: Int) = {
    require(n > 0)
    require(n < 3000)

    var romanNumber = ""
    val digits = n.toString.map(_.asDigit)
    digits.reverse.zipWithIndex.foreach {
      case (digit, power) => romanNumber =  romanize(digit, power) + romanNumber
    }

    romanNumber
  }


  override def numerize(n: String) : Int = {
    numerize(n, 0)
  }

  private def romanize(digit: Int, power: Int) : String = {
    val rPower = romanPowers(power)

    digit match {
      case 0 => ""
      case 4 => rPower.lowerSymbol + rPower.midSymbol
      case 5 => rPower.midSymbol
      case 9 => rPower.lowerSymbol + rPower.upperSymbol
      case _ => if (digit < 5) rPower.lowerSymbol * digit
                else           rPower.midSymbol + romanize(digit - 5, power)
    }

  }

  private case class RomanPower(val lowerSymbol : String, 
                                val midSymbol : String, 
                                val upperSymbol: String)

  private val romanPowers : List[RomanPower] = {
    RomanPower("I", "V", "X") ::
    RomanPower("X", "L", "C") ::
    RomanPower("C", "D", "M") ::
    RomanPower("M", "-", "-") ::
    List.empty
  }

  private val digitsByCheckingOrder = List(9, 4, 5, 8, 7, 6, 3, 2, 1, 0)

  private def numerize(n: String, power: Int) : Int = {

    n match {
      case "" => 0
      case _ => {
        for (digit <- digitsByCheckingOrder) {
          val romanNumber = romanize(digit, power)
          if (n.endsWith(romanNumber)) {
            val rest = n.dropRight(romanNumber.length)
            val acc = digit * pow(10, power).toInt
            return acc + numerize(rest, power + 1)
          }
        }
        throw new InvalidParameterException()
      }
    }
  }
}