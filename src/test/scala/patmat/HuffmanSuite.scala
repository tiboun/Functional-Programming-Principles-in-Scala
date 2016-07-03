package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a longer text should be identity") {
    new TestTrees {
      private val toList: List[Char] = "someText".toList
      val codeTree = createCodeTree(toList)
      assert(decode(codeTree, encode(codeTree)(toList)) === toList)
    }
  }

  test("count times of chars") {
    val charsByTimes: List[(Char, Bit)] = times("abcdabcaba".toList)
    assert(charsByTimes.find(_._1 == 'a') == Some(('a',4)))
    assert(charsByTimes.find(_._1 == 'b') == Some(('b',3)))
    assert(charsByTimes.find(_._1 == 'c') == Some(('c',2)))
    assert(charsByTimes.find(_._1 == 'd') == Some(('d',1)))
  }

  test("make ordered leaf list") {
    val charsByTimes: List[(Char, Bit)] = times("abcdabcaba".toList)
    val leafList: List[Leaf] = makeOrderedLeafList(charsByTimes)
    assert(leafList(0).char == 'd')
    assert(leafList(1).char == 'c')
    assert(leafList(2).char == 'b')
    assert(leafList(3).char == 'a')
  }

  test("convert to code table") {
    new TestTrees {
      val toList: List[Char] = "someText".toList
      val codeTree = createCodeTree(toList)
      val codeTable: CodeTable = convert(codeTree)
      println(codeTable)
    }
  }
}
