package thistle.predicates

import thistle.core.{ElementPredicate, ElementState}

object Character {
  def isCharacter(char: Char): ElementPredicate[Char] =
    (ms: ElementState[Char]) => ms.value == char

  def isInCharacterRange(startChar: Char, endChar: Char): ElementPredicate[Char] = 
    (m: ElementState[Char]) => {
      val charCode = m.value.toByte

      charCode > startChar.toByte && charCode < endChar.toByte
    }

  val digit =
    isInCharacterRange('0', '9')

  val upperCaseLetter =
    isInCharacterRange('A', 'Z')

  val lowerCaseLetter =
    isInCharacterRange('a', 'z')

  val letter =
    upperCaseLetter || lowerCaseLetter

  val vowel: ElementPredicate[Char] =
    (ms: ElementState[Char]) =>
      Set('a', 'e', 'i', 'o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y').contains(ms.value)

  val underscore =
    isCharacter(new Character(95))

  val wordCharacter = 
    letter || digit || underscore

  val whiteSpace =
    (m: ElementState[Char]) => m.value.toByte < 33
}