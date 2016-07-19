package forcomp

import java.io.InputStream

import scala.collection.parallel.ParSeq
import scala.io.Source

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    * how often the character appears.
    * This list is sorted alphabetically w.r.t. to the character in each pair.
    * All characters in the occurrence list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not sorted
    * is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character should not be
    * in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as the
    * same character, and are represented as a lowercase character in the occurrence list.
    *
    * Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(c => c).mapValues(_.size).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = if (s.isEmpty) wordOccurrences("") else wordOccurrences(s.reduce(_ + _))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    * the words that have that occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val linuxwords: InputStream = getClass.getResourceAsStream("/forcomp/linuxwords.txt")
    def mergeOccurence(wordWithOccurence: List[(Occurrences, String)], result: Map[Occurrences, List[Word]]): Map[Occurrences, List[Word]] = {
      wordWithOccurence match {
        case Nil => result
        case aWordWithOccurence :: rest => {
          val list: List[Word] = result.getOrElse(aWordWithOccurence._1, List())
          val updatedMap: Map[Occurrences, List[Word]] = result.updated(aWordWithOccurence._1, aWordWithOccurence._2 :: list)
          mergeOccurence(rest, updatedMap)
        }
      }
    }
    val wordWithOccurence: List[(Occurrences, String)] = Source.fromInputStream(linuxwords).getLines().map(l => {
      wordOccurrences(l).sorted -> l
    }).toList
    mergeOccurence(wordWithOccurence, Map())
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences(wordOccurrences(word).sorted)
  }

  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinations(subSets: ParSeq[Occurrences], acc: ParSeq[Occurrences] = ParSeq(List(), occurrences)): List[Occurrences] = {
      subSets.isEmpty match {
        case true => acc.map(_.sorted).toSet.toList
        case _ => {
          val newCombinations: ParSeq[List[(Char, Int)]] = for {
            occurences <- subSets
            occurence <- occurences
          } yield {
            val newOccurence: List[(Char, Int)] = (occurence._1, occurence._2 - 1) :: occurences.filterNot(_._1 == occurence._1)
            newOccurence.filterNot(_._2 == 0)
          }
          val newCombinationsWithoutEmpty: ParSeq[Occurrences] = newCombinations.filterNot(_.isEmpty)
          combinations(newCombinationsWithoutEmpty, newCombinationsWithoutEmpty ++ acc)
        }
      }
    }
    combinations(List(occurrences).par)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val occurenceAsMap: Map[Char, Int] = y.toMap
    x.map {
      case (c, occurence) => {
        c -> (occurence - occurenceAsMap.getOrElse(c, 0))
      }
    }.filterNot(_._2 == 0)
  }

  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my),
    * List(en, my, as),
    * List(man, yes),
    * List(men, say),
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val sentenceOccurrences1: Occurrences = sentenceOccurrences(sentence)
    val time1: Long = System.currentTimeMillis()
    val combinations1: List[Occurrences] = combinations(sentenceOccurrences1)
    val time2: Long = System.currentTimeMillis()
    println("combinations :" + (time2 - time1))
    val allValidWords: List[(Occurrences, Option[List[Word]])] = for {
      combination <- combinations1
    } yield {
      combination -> dictionaryByOccurrences.get(combination.sorted)
    }
    val time3: Long = System.currentTimeMillis()
    println("allValidWords :" + (time3 - time2))
    val allValidWordsWithoutEmpty: List[(Occurrences, List[Word])] = allValidWords.flatMap {
      case (occurence, Some(l)) => Some(occurence -> l)
      case _ => None
    }.filterNot(_._2.isEmpty)
    val time4: Long = System.currentTimeMillis()
    println("allValidWordsWithoutEmpty :" + (time4 - time3))
    val wordsWithUpdatedOccurence: List[(Occurrences, List[(Occurrences, List[Word])])] = allValidWordsWithoutEmpty.map(word => subtract(sentenceOccurrences1, word._1) -> List(word))
    val words: List[List[List[Word]]] = combineAllValidWords(allValidWordsWithoutEmpty, wordsWithUpdatedOccurence.par)
    val time5: Long = System.currentTimeMillis()
    println("combineAllValidWords :" + (time5 - time4))
    val toList: List[List[Sentence]] = words.map(buildSentences(_))
    val time6: Long = System.currentTimeMillis()
    println("buildSentences :" + (time6 - time5))
    val validSentences: List[Sentence] = toList.flatMap(t => t).toSet.toList
    if (validSentences.isEmpty)
      List(List())
    else
      validSentences
  }

  private def combineAllValidWords(allValidWords: List[(Occurrences, List[Word])],
                                   workingCombinations: ParSeq[(Occurrences, List[(Occurrences, List[Word])])],
                                   acc: ParSeq[List[List[Word]]] = ParSeq.empty
                                  ): List[List[List[Word]]] = {
    if (workingCombinations.size == 0) {
      acc.map(_.reverse).toList
    } else {
      val (allValidCombinations, allInvalidCombinations) = workingCombinations.partition(_._1.isEmpty)
      val partialCombinations: ParSeq[(Occurrences, List[(Occurrences, List[Word])])] = for {
        (currentOccurence, currentCombinations) <- allInvalidCombinations
        oneValidWord <- allValidWords
        combinationsOfCurrentOccurence = combinations(currentOccurence)
        if combinationsOfCurrentOccurence.exists(_.sorted == oneValidWord._1.sorted)
      } yield {
        subtract(currentOccurence, oneValidWord._1) -> (oneValidWord :: currentCombinations)
      }
      combineAllValidWords(allValidWords, partialCombinations, allValidCombinations.map(_._2.map(_._2)) ++ acc)
    }
  }

  private def buildSentences(sentencePossibilities: List[List[Word]], acc: List[Sentence] = List(List())): List[Sentence] = {
    sentencePossibilities match {
      case Nil => acc.map(_.reverse)
      case words :: rest => {
        val addedWords: List[Sentence] = for {
          sentence <- acc
          word <- words
        } yield {
          word :: sentence
        }
        buildSentences(rest, addedWords)
      }
    }
  }
}
