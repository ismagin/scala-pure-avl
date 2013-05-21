package org.sschoener

import org.sschoener.monad._
import org.sschoener.avl._

import AVLNode._
import State._

object MonadExpansion {
    def main(args: Array[String]) {
        type AVLI = AVLTree[Int]
        // threading state manually
        val avl = AVLNode(4)
        val avl2 = insert(2)(avl)
        val avl3 = insert(3)(avl2)
        val avl4 = insert(7)(avl3)
        val avl5 = insert(-2)(avl4)
        val avl6 = insert(12)(avl5)

        val expandedMonad10 = 
            new State[AVLI, Unit] {
                def run(t: AVLI) = {
                    // first part of the flatMap
                    val (s1, a) = (t, t) // result of get 

                    // second part of the flatMap
                    val (s2, b) = (insert(2)(a), ()) // result of put 
                    (s2, () /* result of map */)
                }
            }
       
        val (emavl10, _) = expandedMonad10.run(AVLNode(4))
        assert(emavl10 == avl2)


        val expandedMonad9 = 
            new State[AVLI, Unit] {
                def run(t: AVLI) = {
                    // first part of the flatMap
                    val (s1, a) = (t, t) // result of get 

                    // second part of the flatMap
                    val (s2, b) = new State[AVLI, Unit] {
                        def run(t: AVLI) = // put
                            (insert(2)(a), ()) 
                    }.run(s1)
                    (s2, () /* result of map */)
                }
            }
       
        val (emavl9, _) = expandedMonad9.run(AVLNode(4))
        assert(emavl9 == avl2)



        val expandedMonad8 = 
            new State[AVLI, Unit] {
                def run(t: AVLI) = {
                    // first part of the flatMap
                    val (s1, a) = (t, t) // result of get 

                    // second part of the flatMap
                    new State[AVLI, Unit] { // expanding map
                        def run(s: AVLI) = {
                            val (s2, b) = new State[AVLI, Unit] {
                                def run(t: AVLI) = // put
                                    (insert(2)(a), ()) 
                            }.run(s)
                            (s2, () /* result of map */)
                        }
                    }.run(s1)
                }
            }
       
        val (emavl8, _) = expandedMonad8.run(AVLNode(4))
        assert(emavl8 == avl2)

        val expandedMonad7 = 
            new State[AVLI, Unit] {
                def run(t: AVLI) = {
                    // first part of the flatMap
                    val (s1, a) = (t, t) // result of get 

                    // second part of the flatMap
                    { avl: AVLI =>
                         new State[AVLI, Unit] { // expanding map
                            def run(s: AVLI) = {
                              val (s2, b) = new State[AVLI, Unit] {
                                    def run(t: AVLI) = // put
                                        (insert(2)(avl), ()) 
                                }.run(s)
                                (s2, () /* result of map */)
                            }
                        }   
                    }.apply(a).run(s1)
                }
            }
       
        val (emavl7, _) = expandedMonad7.run(AVLNode(4))
        assert(emavl7 == avl2)



        val expandedMonad6 = 
            new State[AVLI, Unit] {
                def run(t: AVLI) = {
                    // first part of the flatMap
                    val (s1, a) = new State[AVLI, AVLI] {
                        def run(t: AVLI) = (t, t) // get
                    }.run(t)

                    // second part of the flatMap
                    { avl: AVLI =>
                        new State[AVLI, Unit] { // expanding map
                            def run(s: AVLI) = {
                              val (s2, b) = new State[AVLI, Unit] {
                                    def run(t: AVLI) = // put
                                        (insert(2)(avl), ()) 
                                }.run(s)
                                (s2, {_: Unit => ()}.apply(a))
                            }
                        }   
                    }.apply(a).run(s1)
                }
            }
       
        val (emavl6, _) = expandedMonad6.run(AVLNode(4))
        assert(emavl6 == avl2)



        val expandedMonad5 = 
            State { s: AVLI => // expanding flat map

                // first part of the flatMap
                val (s1, a) = State {
                    s: AVLI => (s,s) // get
                }.run(s)
            
                // second part of the flatMap
                { avl: AVLI =>
                    State { s: AVLI => // expanding map
                        val (s2, b) = State { _: AVLI => // put
                            (insert(2)(avl), ())
                        }.run(s)
                        (s2, {_: Unit => ()}.apply(a))
                    }
                }.apply(a).run(s1)
            }
       
        val (emavl5, _) = expandedMonad5.run(AVLNode(4))
        assert(emavl5 == avl2)

        // just the first insertion
        val expandedMonad4 =
            State { s: AVLI => // expanding flat map

                // first part of the flatMap
                val (s1, a) = State {
                    s: AVLI => (s,s) // get
                }.run(s)
            
                // second part of the flatMap
                { avl: AVLI =>
                    State { _: AVLI => // put
                        (insert(2)(avl),())
                    } map { _ => ()}
                }.apply(a).run(s1)
            }
        
        val (emavl4, _) = expandedMonad4.run(AVLNode(4))
        assert(emavl4 == avl2)
 
        // using the state trait
        // these are just the first two insertions
        val expandedMonad3 =
            State { s: AVLI => (s, s) // get
            } flatMap { avl: AVLI => // current state
                State { _: AVLI => // put
                    (insert(2)(avl), ())
                } flatMap { _ =>
                    State { s: AVLI => (s, s) // get
                    } flatMap { avl2 =>
                        State { _: AVLI => // put
                            (insert(3)(avl2),())
                        } map {_ => ()}
                    }
                }
            }
        val (emavl3, _) = expandedMonad3.run(AVLNode(4))
        assert(emavl3 == avl3)

        // using put & get to make things easier
        val expandedMonad2 =
            get flatMap { avl:AVLI => // current state
              put (insert(2)(avl)) flatMap { _ =>
                get flatMap { avl2 =>
                  put (insert(3)(avl2)) flatMap { _ =>
                    get flatMap { avl3 =>
                      put (insert(7)(avl3)) flatMap { _ =>
                        get flatMap { avl4 =>
                          put (insert(-2)(avl4)) flatMap { _ =>
                            get flatMap { avl5 =>
                              put (insert(12)(avl5)) map { _ => () }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
        val (emavl2, _) = expandedMonad2.run(AVLNode(4))

        // using modify to simplify the code
        val expandedMonad = modify { insert(2) } flatMap { _ =>
            modify { insert(3) } flatMap { _ =>
                modify { insert(7) } flatMap { _ =>
                    modify { insert(-2) } flatMap { _ =>
                       modify { insert(12) } map { _ => () }
                    }
                }
            }
        }
        val (emavl, _) = expandedMonad.run(AVLNode(4))

        // using a for-comprehension
        val monad = for {
            _ <- modify { insert(2) }
            _ <- modify { insert(3) }
            _ <- modify { insert(7) }
            _ <- modify { insert(-2) }
            _ <- modify { insert(12) }
        } yield ()

        val (monadAvl, _) = monad.run(AVLNode(4))
        assert(monadAvl == avl6)
        assert(monadAvl == emavl)
        assert(monadAvl == emavl2)
    }
}
