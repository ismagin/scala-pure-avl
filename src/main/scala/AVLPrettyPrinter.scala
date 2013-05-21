package org.sschoener.avl


object AVLPrettyPrinter {
    /**
     * Pretty-prints an AVL-tree in a very primitive fashion.
     */
    def pretty[A](t: AVLTree[A]): String = {
        def mkList(s: String, n: Int): List[String] = n match {
            case 0 => List()
            case n => s :: mkList(s, n - 1)
        }
        def pp(t: AVLTree[A]): List[String] = {
            t match {
                case AVLNil => List[String]()
                case AVLNode(v, AVLNil, AVLNil) => List(v.toString)
                case AVLNode(v, l, r) => {
                    val lp = pp(l)
                    val rp = pp(r)
                    val maxlen = math.max(lp.length, rp.length)
                    def fixlen(xs: List[String]) = {
                        val xlen = xs.length
                        if (xlen < maxlen) {
                            val entrylen = if (xlen == 0) 0
                                           else xs.head.length
                            xs ::: mkList(" " * entrylen,
                                           maxlen - xlen)
                        } else xs
                    }
                    val lpfix = fixlen(lp)
                    val rpfix = fixlen(rp)
                    val vstr = v.toString
                    val vlen = vstr.length
                    val llen = if (l == AVLNil) 0 else lp.head.length
                    val rlen = if (r == AVLNil) 0 else rp.head.length
                    
                    val padLeft = " " * (llen/2)
                    val conLeft = "+" + ("-" * (llen/2))
                    val conRight = ("-" * (rlen/2)) + "+"
                    val padRight = " " * (rlen/2)
                    val s = padLeft + conLeft + vstr +
                            conRight + padRight
                    val pipeLeft = padLeft + "|" + (" " * (llen/2))
                    val pipeRight = (" " * (rlen/2)) + "|" + padRight
                    val pipes = pipeLeft + (" " * vlen) + pipeRight
                    s :: pipes :: (lpfix zip rpfix).map(x =>
                            x._1 + (" " * vlen) + x._2) 
                }
            }
        }
        pp(t).mkString("\n")
    }
}

