package buccaneer

/** A command line interface abstraction.
  * A command line interface is essentially a Map
  * from command shapes to their functions.
  */
object Cli {
  type Cli[A] = Map[Expr, Command[A]]
  def apply[A](commands: Command[A]*): Cli[A] = commands.map(c => c.expr -> c).toMap
}
