package predicates

import core.{MatchPredicate, ElementState}

object Characters {
	
	def isCharacter(char : Char) : MatchPredicate[Char] =
		(ms : ElementState[Char]) => ms.value == char

	def isInCharacterRange(startChar : Char, endChar : Char) : MatchPredicate[Char] = 
		(m : ElementState[Char]) => {
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

	val underscore =
		isCharacter(new Character(95))

	val wordCharacter = 
		letter || digit || underscore

	val whiteSpace =
		(m : ElementState[Char]) => m.value.toByte < 33
}