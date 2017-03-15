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
    override def toString = value + "/" + (mask & ((1 << Tree.maxDepth) - 1)).toHexString
}

object Tree {
    val maxDepth = 4

    def AddValue(value: Int, root: Node): Node = {

        def AddValueImpl(value: Int, root: Node, depth: Int): Node = {
            if (depth <= Tree.maxDepth) {
                val isOne = getBit(value, Tree.maxDepth - depth) == 1
                root match {
                    case Tree(l, r) if !isOne =>
                        new Tree(AddValueImpl(value, l, depth + 1), r)
                    case Tree(l, r) if isOne =>
                        new Tree(l, AddValueImpl(value, r, depth + 1))

                    case Nil if !isOne =>
                        new Tree(AddValueImpl(value, Nil, depth + 1), Nil)
                    case Nil if isOne =>
                        new Tree(Nil, AddValueImpl(value, Nil, depth + 1))
                }
            } else {
                new Leaf(value)
            }
        }

        AddValueImpl(value, root, 1)
    }

    def GetDisallowedRanges(value: Int, trie: Node, min: Boolean, max: Boolean): List[Range] = {

        def GetDisallowedRangesImpl(value: Int, trie: Node, min: Boolean, max: Boolean,
                                    depth: Int): List[Range] = {
            if (depth > Tree.maxDepth) {
                return List()
            } else {
                val bitPos = Tree.maxDepth - depth
                val lastBit = 1 << bitPos
                val mask = ~(lastBit - 1)
                val isOne = getBit(value, bitPos) == 1
                trie match {
                    case Tree(Nil, r) if min || (!min && !max) =>
                        Range((value ^ lastBit) & mask, mask) ::
                        GetDisallowedRangesImpl(value, r, min, max, depth + 1)
                    case Tree(l, Nil) if max || (!max && !min) =>
                        Range((value ^ lastBit) & mask, mask) ::
                        GetDisallowedRangesImpl(value, l, min, max, depth + 1)
                    case Tree(l, r) if isOne =>
                        GetDisallowedRangesImpl(value, r, min, max, depth + 1)
                    case Tree(l, r) if !isOne =>
                        GetDisallowedRangesImpl(value, l, min, max, depth + 1)
                }
            }
        }
        GetDisallowedRangesImpl(value, trie, min, max, 1)
    }


    def GetAllowedRanges(minValue: Int, maxValue: Int, trie: Node): List[Range] = {

        def skipCommonPrefix(minValue: Int, maxValue: Int, trie: Node,
                             depth: Int): (Node, Int) = {
            if (depth > Tree.maxDepth) {
                (trie, depth)
            } else {
                val bitPos = Tree.maxDepth - depth
                val bitMin = getBit(minValue, bitPos)
                val bitMax = getBit(maxValue, bitPos)
                if (bitMin != bitMax) {
                    (trie, depth)
                } else {
                    trie match {
                        case Tree(l, r)  =>
                            if (bitMin == 1) {
                                skipCommonPrefix (minValue, maxValue, r, depth + 1)
                            } else {
                                skipCommonPrefix(minValue, maxValue, l, depth + 1)
                            }
                    }
                }
            }
        }

        def GetAllowedRangesImpl(value: Int, trie: Node, min: Boolean,
                                 max: Boolean, depth: Int): List[Range] = {
            if (depth > Tree.maxDepth) {
                List(Range(value, -1))
            } else {
                val bitPos = Tree.maxDepth - depth
                val isOne = getBit(value, bitPos) == 1
                val lastBit = 1 << (bitPos)
                val mask = ~(lastBit - 1)
                trie match {
                    case Tree(l, Nil) if min =>
                        Range((value ^ lastBit) & mask, mask) ::
                        GetAllowedRangesImpl(value, l, min, max, depth + 1)
                    case Tree(Nil, r) if max =>
                        Range((value ^ lastBit) & mask, mask) ::
                        GetAllowedRangesImpl(value, r, min, max, depth + 1)
                    case Tree(l, r) if isOne =>
                        GetAllowedRangesImpl(value, r, min, max, depth + 1)
                    case Tree(l, r) if !isOne =>
                        GetAllowedRangesImpl(value, l, min, max, depth + 1)
                }
            }
        }

        val (root, depth) = skipCommonPrefix(minValue, maxValue, trie, 1)
        val fromMin = GetAllowedRangesImpl(minValue, root, true, false, depth)
        val fromMax = GetAllowedRangesImpl(maxValue, root, false, true, depth)
        return fromMin ::: fromMax
    }

    private def getBit(value: Int, pos: Int) = {
        if ((value & (1 << pos)) != 0) 1 else 0
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
    Tree.GetDisallowedRanges(2, t2, false, false) foreach println
    val disallowed2 = Tree.GetDisallowedRanges(5, t2, false, false)
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
