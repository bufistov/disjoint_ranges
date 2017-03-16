class Node {
    override def toString: String = {
        this match {
            case Nil => "Nil"
            case Tree(l, r) => "{"  + l.toString + " . " + r.toString + " }"
            case Leaf(v) => v.toString
        }
    }
}

object Nil extends  Node
case class Tree(val left: Node, val right: Node) extends Node
case class Leaf(value: Int) extends Node

case class Range(val value: Int, val mask: Long) {
    override def toString = value + "/" +
                            (mask & ((1L << Tree.maxDepth) - 1L)).toHexString
}

object Tree {
    val maxDepth = 4

    def AddValue(value: Int, trie: Node): Node = {

        def AddValueImpl(value: Int, trie: Node, depth: Int): Node = {
            if (depth > Tree.maxDepth) {
                new Leaf(value)
            } else {
                val isOne = GetBit(value, Tree.maxDepth - depth) == 1
                trie match {
                    case Tree(l, r) if !isOne =>
                        new Tree(AddValueImpl(value, l, depth + 1), r)
                    case Tree(l, r) if isOne =>
                        new Tree(l, AddValueImpl(value, r, depth + 1))

                    case Nil if !isOne =>
                        new Tree(AddValueImpl(value, Nil, depth + 1), Nil)
                    case Nil if isOne =>
                        new Tree(Nil, AddValueImpl(value, Nil, depth + 1))
                }
            }
        }

        AddValueImpl(value, trie, 1)
    }

    def GetDisallowedRanges(value: Int, trie: Node,
                            min: Boolean, max: Boolean): List[Range] = {
        GetRangesImpl(value, trie, min, max, allowed = false, 1)
    }


    def GetAllowedRanges(minValue: Int, maxValue: Int, trie: Node): List[Range] = {

        def SkipCommonPrefix(minValue: Int, maxValue: Int, trie: Node,
                             depth: Int): (Node, Int) = {
            if (depth > Tree.maxDepth) {
                (trie, depth)
            } else {
                val bitPos = Tree.maxDepth - depth
                val bitMin = GetBit(minValue, bitPos)
                val bitMax = GetBit(maxValue, bitPos)
                if (bitMin != bitMax) {
                    (trie, depth)
                } else {
                    trie match {
                        case Tree(l, r)  =>
                            if (bitMin == 1) {
                                SkipCommonPrefix (minValue, maxValue, r, depth + 1)
                            } else {
                                SkipCommonPrefix(minValue, maxValue, l, depth + 1)
                            }
                    }
                }
            }
        }

        val (root, depth) = SkipCommonPrefix(minValue, maxValue, trie, 1)
        val fromMin = GetRangesImpl(minValue, root, min = true, max = false,
                                    allowed = true, depth)
        val fromMax = GetRangesImpl(maxValue, root, min = false, max = true,
                                    allowed = true, depth)
        return fromMin ::: fromMax
    }

    private def GetBit(value: Int, pos: Int) = {
        if ((value & (1 << pos)) != 0) 1 else 0
    }

    private def GetRangesImpl(value: Int, trie: Node,
                              min: Boolean, max: Boolean, allowed: Boolean,
                              depth: Int): List[Range] = {
        if (depth > Tree.maxDepth) {
            if (allowed) {
                List(Range(value, -1))
            } else {
                List()
            }
        } else {
            val bitPos = Tree.maxDepth - depth
            val lastBit = 1 << bitPos
            val mask = ~(lastBit - 1)
            val isOne = GetBit(value, bitPos) == 1
            trie match {
                case Tree(Nil, r) if (!allowed && min) || (allowed && max) =>
                    Range((value ^ lastBit) & mask, mask) ::
                    GetRangesImpl(value, r, min, max, allowed, depth + 1)
                case Tree(l, Nil) if (!allowed && max) || (allowed && min) =>
                    Range((value ^ lastBit) & mask, mask) ::
                    GetRangesImpl(value, l, min, max, allowed, depth + 1)
                case Tree(l, r) if isOne =>
                    GetRangesImpl(value, r, min, max, allowed, depth + 1)
                case Tree(l, r) if !isOne =>
                    GetRangesImpl(value, l, min, max, allowed, depth + 1)
            }
        }
    }
}

object Main extends App {
    val t1 = Tree.AddValue(2, Nil)
    val t2 = Tree.AddValue(5, t1)

    println(t2.toString)

    println("Disallowed [2, 5] ranges:")
    Tree.GetDisallowedRanges(2, t2, true, false) foreach println
    Tree.GetDisallowedRanges(5, t2, false, true) foreach println

    println("Disallowed [2],[5] ranges:")
    Tree.GetDisallowedRanges(2, t2, true, true) foreach println
    val disallowed2 = Tree.GetDisallowedRanges(5, t2, true, true)
    disallowed2 foreach println

    println("Allowed [2,5] ranges:")
    Tree.GetAllowedRanges(2, 5, t2) foreach println

    println("Allowed [2,2] ranges:")
    Tree.GetAllowedRanges(2, 2, t2) foreach println

    val t3 = Tree.AddValue(12, t2)
    println(t3.toString)
    println("Allowed [5, 12] ranges")
    Tree.GetAllowedRanges(5, 12, t3) foreach println

    val t4 = Tree.AddValue(0, Tree.AddValue(15, Nil))
    println("Allowed [0, 15] ranges")
    Tree.GetAllowedRanges(0, 15, t4) foreach println
}
