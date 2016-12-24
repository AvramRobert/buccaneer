package core

import core.Command._
import scalaz.{State, _}
import Binary.treeSyntax
import scala.language.reflectiveCalls
import Denot._
import Reified._
import scalaz.syntax.validation._

object Command {
  type TypedC[A] = IndexedState[List[String], List[String], Result[A]]

  def apply(label: String): Cmd0 = new Cmd0(Binary.lift(id(label)))
}

private[core]
sealed trait CmdBld[+A] {
  protected def coerce[B](f: String => Result[B]): TypedC[B] = coerce(Reified(f))

  protected def coerce[B](proof: Reified[B]): TypedC[B] =
    for {
      types <- State.get[List[String]]
      _ <- State.modify[List[String]] { list =>
        if (list.isEmpty) list
        else list.tail
      }
      currentType = types.headOption getOrElse ""
    } yield proof(currentType)

  // Caution: Doing the computation within `fa` reverses the order of parameter application when running the Monad
  // I.e given a command.param[Int].param[String] <- List("1", "a"), it will feed `1` and `a` in reverse => first "a" then "1"
  // If the computation happens within `f`, then the order is correct.
  protected lazy val applier: Apply[TypedC] = new Apply[TypedC] {
    override def ap[B, C](fa: => TypedC[B])(f: => TypedC[B => C]): TypedC[C] =
      for {
        vf <- f
        va <- fa
      } yield va ap vf

    override def map[B, C](fa: TypedC[B])(f: B => C): TypedC[C] = fa map (_ map f)
  }

  protected def proof[B](implicit r: Reified[B]): Reified[B] = r

  protected def txt(contents: String, syntax: Tree[Denot]): Tree[Denot] = syntax mapLast (_ mapDocs (_ mapMsg (_ => contents)))

  protected def opt[C[_] <: CmdBld[_], B](label: Sym)(f: Tree[Denot] => C[B]): C[B] = (f compose Binary.lift[Denot]) (id(label))

  protected def arg[C[_] <: CmdBld[_], B](proof: Reified[B])(f: (TypedC[B], Tree[Denot]) => C[B]): C[B] = f(coerce(proof), Binary.lift(typing(proof)))

  protected def assign[C[_] <: CmdBld[_], B](label: Sym, proof: Reified[B])(f: (TypedC[B], Tree[Denot]) => C[B]): C[B] = {
    val newProof = Implicits.association(label)(proof)
    f(coerce(newProof), Binary.lift(typedId(label, newProof)))
  }
}

private[core]
final class Cmd[A](types: TypedC[A], val syntax: Tree[Denot]) extends CmdBld[A] {
  def run(args: List[String]): Result[A] = (types run args)._2
}
// This is essentially a holey monoid what you've done here
private[core]
final class Cmd0(syntax0: Tree[Denot]) extends CmdBld[Nothing] {

  def msg(contents: String): Cmd0 = new Cmd0(txt(contents, syntax0))

  def option(label: Sym): Cmd0 = opt[({type α[x] = Cmd0})#α, Nothing](label) { syntax => new Cmd0(syntax0 affix syntax) }

  def argument[A: Reified]: Cmd1[A] = arg(proof) { (types, syntax) => new Cmd1(types, syntax0 affix syntax) }

  def assignment[A: Reified](label: Sym): Cmd1[A] = assign(label, proof) { (types, syntax) => new Cmd1(types, syntax0 affix syntax) }

  def apply[A](f: () => A): Cmd[A] = new Cmd(coerce(_ => f().successNel), syntax0)

  final class Cmd1[A](types1: TypedC[A], syntax1: Tree[Denot]) extends CmdBld[A] {

    def msg(contents: String): Cmd1[A] = new Cmd1(types1, txt(contents, syntax1))

    def option(label: Sym): Cmd1[A] = opt(label) { syntax => new Cmd1(types1, syntax1 affix syntax) }

    def argument[B: Reified]: Cmd2[B] = arg(proof) { (types, syntax) => new Cmd2(types, syntax1 affix syntax) }

    def assignment[B: Reified](label: Sym): Cmd2[B] = assign(label, proof) { (types, syntax) => new Cmd2(types, syntax1 affix syntax) }

    def apply[B](f: A => B): Cmd[B] = new Cmd(applier.map(types1)(f), syntax1)

    final class Cmd2[B](types2: TypedC[B], syntax2: Tree[Denot]) extends CmdBld[B] {

      def msg(contents: String): Cmd2[B] = new Cmd2(types2, txt(contents, syntax2))

      def option(label: Sym): Cmd2[B] = opt(label) { syntax => new Cmd2(types2, syntax2 affix syntax1) }

      def argument[C: Reified]: Cmd3[C] = arg(proof) { (types, syntax) => new Cmd3(types, syntax2 affix syntax) }

      def assignment[C: Reified](label: Sym): Cmd3[C] = assign(label, proof) { (types, syntax) => new Cmd3(types, syntax2 affix syntax) }

      def apply[C](f: (A, B) => C): Cmd[C] = new Cmd(applier.apply2(types1, types2)(f), syntax2)

      final class Cmd3[C](types3: TypedC[C], syntax3: Tree[Denot]) extends CmdBld[C] {

        def msg(contents: String): Cmd3[C] = new Cmd3(types3, txt(contents, syntax3))

        def option(label: Sym): Cmd3[C] = opt(label) { syntax => new Cmd3(types3, syntax3 affix syntax) }

        def argument[D: Reified]: Cmd4[D] = arg(proof) { (types, syntax) => new Cmd4(types, syntax3 affix syntax) }

        def assignment[D: Reified](label: Sym): Cmd4[D] = assign(label, proof) { (types, syntax) => new Cmd4(types, syntax3 affix syntax) }

        def apply[D](f: (A, B, C) => D): Cmd[D] = new Cmd(applier.apply3(types1, types2, types3)(f), syntax3)

        final class Cmd4[D](types4: TypedC[D], syntax4: Tree[Denot]) extends CmdBld[D] {

          def msg(contents: String): Cmd4[D] = new Cmd4(types4, txt(contents, syntax4))

          def option(label: Sym): Cmd4[D] = opt(label) { syntax => new Cmd4(types4, syntax4 affix syntax) }

          def argument[E: Reified]: Cmd5[E] = arg(proof) { (types, syntax) => new Cmd5(types, syntax4 affix syntax) }

          def assignment[E: Reified](label: Sym): Cmd5[E] = assign(label, proof) { (types, syntax) => new Cmd5(types, syntax4 affix syntax) }

          def apply[E](f: (A, B, C, D) => E): Cmd[E] = new Cmd(applier.apply4(types1, types2, types3, types4)(f), syntax4)

          final class Cmd5[E](types5: TypedC[E], syntax5: Tree[Denot]) extends CmdBld[E] {

            def msg(contents: String): Cmd5[E] = new Cmd5(types5, txt(contents, syntax5))

            def option(label: Sym): Cmd5[E] = opt(label) { syntax => new Cmd5(types5, syntax5 affix syntax) }

            def argument[F: Reified]: Cmd6[F] = arg(proof) { (types, syntax) => new Cmd6(types, syntax5 affix syntax) }

            def assignment[F: Reified](label: Sym): Cmd6[F] = assign(label, proof) { (types, syntax) => new Cmd6(types, syntax5 affix syntax) }

            def apply[F](f: (A, B, C, D, E) => F): Cmd[F] = new Cmd(applier.apply5(types1, types2, types3, types4, types5)(f), syntax5)

            final class Cmd6[F](types6: TypedC[F], syntax6: Tree[Denot]) extends CmdBld[F] {

              def msg(contents: String): Cmd6[F] = new Cmd6(types6, txt(contents, syntax6))

              def option(label: Sym): Cmd6[F] = opt(label) { syntax => new Cmd6(types6, syntax6 affix syntax) }

              def argument[G: Reified]: Cmd7[G] = arg(proof) { (types, syntax) => new Cmd7(types, syntax6 affix syntax) }

              def assignment[G: Reified](label: Sym): Cmd7[G] = assign(label, proof) { (types, syntax) => new Cmd7(types, syntax6 affix syntax) }

              def apply[G](f: (A, B, C, D, E, F) => G): Cmd[G] = new Cmd(applier.apply6(types1, types2, types3, types4, types5, types6)(f), syntax6)

              final class Cmd7[G](types7: TypedC[G], syntax7: Tree[Denot]) extends CmdBld[G] {

                def msg(contents: String): Cmd7[G] = new Cmd7(types7, txt(contents, syntax7))

                def option(label: Sym): Cmd7[G] = opt(label) { syntax => new Cmd7(types7, syntax7 affix syntax) }

                def argument[H: Reified]: Cmd8[H] = arg(proof) { (types, syntax) => new Cmd8(types, syntax7 affix syntax) }

                def assignment[H: Reified](label: Sym): Cmd8[H] = assign(label, proof) { (types, syntax) => new Cmd8(types, syntax7 affix syntax) }

                def apply[H](f: (A, B, C, D, E, F, G) => H): Cmd[H] = new Cmd(applier.apply7(types1, types2, types3, types4, types5, types6, types7)(f), syntax7)

                final class Cmd8[H](types8: TypedC[H], syntax8: Tree[Denot]) extends CmdBld[H] {

                  def msg(contents: String): Cmd8[H] = new Cmd8(types8, txt(contents, syntax8))

                  def option(label: Sym): Cmd8[H] = opt(label) { syntax => new Cmd8(types8, syntax8 affix syntax) }

                  def argument[I: Reified]: Cmd9[I] = arg(proof) { (types, syntax) => new Cmd9(types, syntax8 affix syntax) }

                  def assignment[I: Reified](label: Sym): Cmd9[I] = assign(label, proof) { (types, syntax) => new Cmd9(types, syntax8 affix syntax) }

                  def apply[I](f: (A, B, C, D, E, F, G, H) => I): Cmd[I] = new Cmd(applier.apply8(types1, types2, types3, types4, types5, types6, types7, types8)(f), syntax8)

                  final class Cmd9[I](types9: TypedC[I], syntax9: Tree[Denot]) extends CmdBld[I] {

                    def msg(contents: String): Cmd9[I] = new Cmd9(types9, txt(contents, syntax9))

                    def option(label: Sym): Cmd9[I] = opt(label) { syntax => new Cmd9(types9, syntax9 affix syntax) }

                    def argument[J: Reified]: Cmd10[J] = arg(proof) { (types, syntax) => new Cmd10(types, syntax9 affix syntax) }

                    def assignment[J: Reified](label: Sym): Cmd10[J] = assign(label, proof) { (types, syntax) => new Cmd10(types, syntax9 affix syntax) }

                    def apply[J](f: (A, B, C, D, E, F, G, H, I) => J): Cmd[J] = new Cmd(applier.apply9(types1, types2, types3, types4, types5, types6, types7, types8, types9)(f), syntax9)

                    final class Cmd10[J](types10: TypedC[J], syntax10: Tree[Denot]) extends CmdBld[J] {
                      def apply[K](f: (A, B, C, D, E, F, G, H, I, J) => K): Cmd[K] = new Cmd(applier.apply10(types1, types2, types3, types4, types5, types6, types7, types8, types9, types10)(f), syntax10)
                    }

                  }

                }

              }

            }

          }

        }

      }

    }

  }

}