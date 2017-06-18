package core

/** A command line interface abstraction.
  * A command line interface is essentially a Map
  * from command shapes to their functions.
  */
object Cli {
  type Cli[A] = Map[Tree[Denot], Cmd[A]]
  def apply[A](commands: Cmd[A]*): Cli[A] = commands.map(c => c.syntax -> c).toMap
}
