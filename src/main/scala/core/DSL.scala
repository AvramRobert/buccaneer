package core

object DSL extends CommandOps with ManOps

object Cli {
  type Cli[A] = Map[Tree[Denot], Cmd[A]]
  def apply[A](commands: Cmd[A]*): Cli[A] = commands.map(c => c.syntax -> c).toMap
}
