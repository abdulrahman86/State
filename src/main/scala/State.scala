
case class State[S,+A](run: S => (S, A)) {

  import State._

  def map[B](f: A => B) : State[S, B] = map2(unit(()))((x, y) => f(x))
  def map2[B, C](s2: State[S, B])(f: (A, B) => C) : State[S, C] =
    flatMap(a => s2 flatMap(b => unit(f(a, b))))
  def flatMap[B](f: A => State[S,B]) : State[S,B] =
    State((s) => {
      val (s2, b)  = run(s)
      f(b) run(s2)
    })
}

object State {

  def unit[S, A](a: A) = State[S, A](s => (s, a))
  def sequence[A, S](x: List[State[S, A]]): State[S, List[A]] =
    x.foldRight(unit[S,List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  def get[S]: State[S, S] = State(s => (s, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield()
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  import State._

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (input => modify[Machine](update(input))))
    s <- get
  } yield (s.coins, s.candies)
}
