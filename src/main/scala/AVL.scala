package org.sschoener.avl

sealed abstract class AVLTree[+A]

/**
 * The empty tree.
 */
case object AVLNil extends AVLTree[Nothing]

/**
 * Composite case
 */
case class AVLNode[+A] private (val label: A,
                       val left: AVLTree[A],
                       val right: AVLTree[A]) extends AVLTree[A] {
    def this(label: A) = this(label, AVLNil, AVLNil)
}


object AVLNode {
    def apply[A](label: A): AVLNode[A] = new AVLNode(label)

    /**
     * Checks that a given tree is indeed AVL.
     */
    private def avlInvariant[A: Ordering](t: AVLTree[A]): Boolean = 
    t match {
        case AVLNil => true
        case AVLNode(_, AVLNil, AVLNil) => true
        case AVLNode(v, l, r) => {
            val ord = implicitly[Ordering[A]]
            val b = balance(t)
            val balanced = (b <= 1) && (-1 <= b)
            val leftOkay = l match {
                case AVLNil => true
                case AVLNode(_, _, _) => avlInvariant(l) &&
                                         ord.lt(max(l), v)
            }
            val rightOkay = r match {
                case AVLNil => true
                case AVLNode(_, _, _) => avlInvariant(r) &&
                                         ord.gt(min(r), v)
            }
            
            balanced && leftOkay && rightOkay
        }
    }

    /**
     * Returns the depth of an AVLTree
     */
    def depth[A](t: AVLTree[A]): Int = t match {
        case AVLNil => 0
        case AVLNode(_,l,r) => 1 + math.max(depth(l), depth(r))
    }

    /**
     * Returns the balance of an AVLTree
     */
    def balance[A](t: AVLTree[A]): Int = t match {
        case AVLNil => 0
        case AVLNode(_,l,r) => depth(l) - depth(r)
    }

    /**
     * Returns whether the AVL tree contains a given value.
     */
    def search[A: Ordering](a: A)(t: AVLTree[A]): Boolean = {
        require(avlInvariant(t), "Tree is not AVL!")
        t match {
            case AVLNil => false
            case AVLNode(b, l, r) => {
                val ord = implicitly[Ordering[A]]
                if (ord.equiv(b, a)) true
                else if (ord.gt(b, a)) search(a)(l)
                else search(a)(r)
            }
        }
    } 
    
    /**
     * Returns the minimum value from a given AVL-tree.
     */
    def min[A: Ordering](t: AVLTree[A]): A = {
        require(t != AVLNil, "can't get min of empty tree")
        t match{
            case AVLNode(v, AVLNil, _) => v
            case AVLNode(_, l, _) => min[A](l)
        }
    }

    /**
     * Returns the maximum value from a given AVL-tree.
     */
    def max[A: Ordering](t: AVLTree[A]): A = {
        require(t != AVLNil, "can't get max of empty tree")
        t match {
            case AVLNode(v, _, AVLNil) => v
            case AVLNode(_, _, r) => max[A](r)
        }
    }

    /**
     * Inserts a value into the AVL-tree.
     */
    def insert[A: Ordering](a: A)(t: AVLTree[A]): AVLTree[A] = {
        require(avlInvariant(t), "Tree is not AVL")
        val tree = t match {
            case AVLNil => AVLNode(a)
            case AVLNode(b, l, r) => {
                val ord = implicitly[Ordering[A]]
                if (ord.equiv(b, a)) t
                else if (ord.gt(b, a)) AVLNode(b, insert(a)(l), r)
                else AVLNode(b, l, insert(a)(r))
            }
        }
        repair(tree)
    } ensuring(r => avlInvariant(r) && search(a)(r))
    
    /**
     * Repairs an AVL-tree after insertion.
     */
    private def repair[A](t: AVLTree[A]): AVLTree[A] = {
        val tree@AVLNode(v, l, r) = t
        val bal = balance(tree)
        if (bal == 2) {
            // insertion into the left child
            val lbal = balance(l)
            if (lbal > 0) {
                // left left case
                rightRotate(tree)
            } else if (lbal < 0) {
                // left right case
                val newl = leftRotate(l)
                rightRotate(AVLNode(v, newl, r))
            } else tree
        } else if (bal == -2) {
            // insertion into the right child
            val rbal = balance(r)
            if (rbal < 0) {
                // right right case
                leftRotate(tree)
            } else if (rbal > 0) {
                // right left case
                val newr = rightRotate(r)
                leftRotate(AVLNode(v, l, newr))
            } else tree
        }
        else tree
    }

    /**
     * Removes a value from the given AVL-tree.
     */
    def remove[A: Ordering](a: A)(t: AVLTree[A]): AVLTree[A] = {
        require(avlInvariant(t), "Tree is not AVL")
        val ord = implicitly[Ordering[A]]
        val tree = t match {
            case AVLNil => AVLNil
            // removing leafs
            case c@AVLNode(b, AVLNil, AVLNil) => {
                if (ord.equiv(b, a)) AVLNil
                else c
            }
            // removing nodes with just one child
            case c@AVLNode(b, l, AVLNil) => {
                if (ord.equiv(b, a)) l
                else if(ord.lt(b, a)) c
                else AVLNode(b, remove(a)(l), AVLNil)
            }
            case c@AVLNode(b, AVLNil, r) => {
                if (ord.equiv(b, a)) r
                else if(ord.gt(b, a)) c
                else AVLNode(b, AVLNil, remove(a)(r))
            }
            // full on deletion
            case AVLNode(b, l, r) if ord.equiv(b,a) => {
                // get left most child of the right tree
                val m = min(r)
                val newr = remove(m)(r)
                AVLNode(m, l, newr)
            }
            case AVLNode(b, l, r) => {
                if (ord.gt(b, a)) AVLNode(b, remove(a)(l), r)
                else AVLNode(b, l, remove(a)(r))
            }
        }
        if (tree == AVLNil) tree
        else repairDelete(tree)
    } ensuring(r => avlInvariant(r) && !search(a)(r))

    /**
     * Repairs an AVL-tree after a deletion.
     */
    private def repairDelete[A: Ordering](t: AVLTree[A]) = {
        val tree@AVLNode(v, l, r) = t
        val bal = balance(tree)
        if (bal == 2) {
            // deletion in right subtree
            val toRot = if (balance(l) < 0) {
                // left right
                val newL = leftRotate(l)
                AVLNode(v, newL, r)
            } else tree // left left
            rightRotate(toRot)
        } else if (bal == -2) {
            // deletion in left subtree
            val toRot = if (balance(r) < 0) {
                // right left
                val newR = rightRotate(r)
                AVLNode(v, l, newR)
            } else tree // right right
            leftRotate(toRot) 
        } else tree
    } ensuring(r => avlInvariant(r))

    /**
     * Performs a left rotation
     */
    private def leftRotate[A](t: AVLTree[A]): AVLTree[A] = t match {
        case AVLNode(x, l, AVLNode(z, m, r)) => {
            AVLNode(z, AVLNode(x, l, m), r)
        }
        case _ => sys.error("tree not suitable for left rotate!")
    }

    /**
     * Performs a right rotation
     */
    private def rightRotate[A](t: AVLTree[A]): AVLTree[A] = t match {
        case AVLNode(x, AVLNode(z, l, m), r) => {
            AVLNode(z, l, AVLNode(x, m, r))
        }
        case _ => sys.error("tree not suitable for right rotate!")
    } 
}
