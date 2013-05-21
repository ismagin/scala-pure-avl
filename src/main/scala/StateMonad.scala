package org.sschoener.monad

/**
 * Implementation of a state moand
 */
trait State[S, +A] {
    /**
     * Runs the function wrapped by the state
     */
    def run(s: S): (S, A)

    /**
     * Function composition
     */
    def map[B](f: A => B): State[S, B] = State { s =>
        val (s1, a) = run(s)
        (s1, f(a))
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
        val (s1, a) = run(s)
        f(a).run(s1)
    }
}

object State {
    def apply[S, A](f: S => (S, A)): State[S, A] = {
        new State[S, A] {
            def run(s: S) = f(s)
        }
    }

    /**
     * Puts a state into the state monad
     */
    def put[S](s: S): State[S, Unit] = State { _ => (s, ()) }

    /**
     * Gets the state from the monad
     */
    def get[S]: State[S, S] = State { s => (s, s) }

    /**
     * Modifies the state in the state monad.
     */
    def modify[S](f: S => S): State[S, Unit] =
        get flatMap { s => put(f(s)) }
    
    /**
     * Applies a function to the state and saves the its value
     * in the value part of the monad.
     */
    def gets[S, A](f: S => A): State[S, A] =
        get map { s => f(s) }

}
