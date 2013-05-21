package org.sschoener.avl.test

import org.sschoener.avl._
import org.scalatest.FunSuite

class AvlTests extends FunSuite {
    import AVLNode._
    test("avl trees basics") {
        val avl = AVLNode(4)
        assert(depth(avl) === 1)
        assert(avl.left === AVLNil)
        assert(balance(avl) === 0)
    }

    test("avl trees search") {
        val avl = insert(4)(insert(1)(AVLNode(2)))
        assert(search(1)(avl))
        assert(search(2)(avl))
        assert(search(4)(avl))
        assert(!search(5)(avl))
    }

    test("avl trees complex interaction") {
        val avl = AVLNil
        val avl2 = insert(2)(avl)
        val avl3 = insert(5)(avl2)
        val avl4 = insert(1)(avl3)
        val avl5 = insert(4)(avl4)
        val avl6 = insert(2)(avl5)
        assert(avl6 === avl5)
        val avl7 = insert(16)(avl6)
        val avl8 = insert(-2)(avl7)
        assert(search(2)(avl8))
        assert(search(5)(avl8))
        assert(search(1)(avl8))
        assert(search(4)(avl8))
        assert(search(16)(avl8))
        assert(search(-2)(avl8))
        val avl9  = remove(2)(avl8)
        val avl10 = remove(5)(avl9)
        val avl11 = remove(1)(avl10)
        val avl12 = remove(4)(avl11)
        val avl13 = remove(2)(avl12)
        assert(avl13 === avl12)
        val avl14 = remove(16)(avl13)
        val avl15 = remove(-2)(avl14)
        assert(avl15 === AVLNil)
    }
}
