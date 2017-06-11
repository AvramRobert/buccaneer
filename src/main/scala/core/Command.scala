package core

import core.Denot.{id, typedId, typing}
import core.Read.Result
import core.Read._
import scalaz.{Apply, IndexedState, State}
import CmdBld._

trait CommandOps {
  implicit def dslFromId(identifier: Identifier): Cmd0 = new Cmd0(Tree(identifier))
  implicit def dslFromTyping[A](typing: Typing[A]): Cmd0#Cmd1[A] = new Cmd0(Leaf).-(typing)
  implicit def dslFromTypedId[A](typedId: TypedIdentifier[A]): Cmd0#Cmd1[A] = new Cmd0(Leaf).-(typedId)

  /** Picks a `Read` instance from implicit scope
    *
    * @param read read instance to pick
    * @tparam A type of `Read`
    * @return `Read` instance
    */
  def proof[A](implicit read: Read[A]): Read[A] = read

  /**
    * Picks a `Read` instance from implicit scope and
    * transforms it into a constrained `Read`
    *
    * @param p predicate constraint on the future value
    * @tparam A type of `Read`
    * @return constrained `Read` instance
    */
  def proofWhen[A: Read](p: A => Boolean): Read[A] = readWhen(proof)(p)

  /** Creates a command identifier.
    *
    * @param label command name as symbol
    * @return identifier denotation
    */
  def command(label: String): Identifier = id(Label(label), isMajor = true)

  /** Creates an option identifier.
    *
    * @param labels a sequence of possible options
    * @return identifier denotation
    */
  def option(labels: String*): Identifier = id(Alternative(labels))

  /** Creates a type argument.
    *
    * @tparam A desired type
    * @return typing denotation
    */
  def argument[A: Read]: Typing[A] = typing(proof)

  /**
    * Creates a constrained type argument.
    *
    * @param p predicate constraint on the future value
    * @tparam A desired type
    * @return typing denotation
    */
  def argument[A: Read](p: (A) => Boolean): Typing[A] = typing(proofWhen(p))

  /** Creates an association between an identifier and a type.
    *
    * @param labels a sequence of possible names for the assignment
    * @tparam A desired type
    * @return typed identifier denotation
    */
  def assignment[A: Read](labels: String*): TypedIdentifier[A] = typedId(Alternative(labels), proof)

  /**
    * Creates a constrained association between an identifier and a type.
    *
    * @param labels a sequence of possible names for the assignment
    * @param p predicate constraint on future value
    * @tparam A desired type
    * @return typed identifier denotation
    */
  def assignment[A: Read](p: A => Boolean)(labels: String*): TypedIdentifier[A] = typedId(Alternative(labels), proofWhen(p))
}

/** A builder DSL for commands, that concretely encodes function arity up to 12 types.
  * The arity stops at 12, because the `apply`-function in scalaz' `Applicative`  only goes up to 12.
  */
private[core]
object CmdBld {
  type Typer[A] = IndexedState[List[String], List[String], Result[A]]

  // Caution: Doing the computation within `fa` reverses the order of parameter application when running the Monad
  // I.e given a command.param[Int].param[String] <- List("1", "a"), it will feed `1` and `a` in reverse => first "a" then "1"
  // If the computation happens within `f`, then the order is correct.
  lazy val applier: Apply[Typer] = new Apply[Typer] {
    override def ap[B, C](fa: => Typer[B])(f: => Typer[B => C]): Typer[C] =
      for {
        vf <- f
        va <- fa
      } yield va ap vf

    override def map[B, C](fa: Typer[B])(f: B => C): Typer[C] = fa map (_ map f)
  }
}

private[core]
sealed trait CmdBld[+A] {
  protected def coerce[B](f: String => Result[B]): Typer[B] = coerce(Read("")(f))

  protected def coerce[B](proof: Read[B]): Typer[B] =
    for {
      types <- State.get[List[String]]
      _ <- State.modify[List[String]] { list =>
        if (list.isEmpty) list
        else list.tail
      }
      currentType = types.headOption getOrElse ""
    } yield proof(currentType)

  protected def makeId[C[_] <: CmdBld[_], B](identifier: Identifier)(f: Tree[Denot] => C[B]): C[B] = (f compose Tree.apply[Denot]) (identifier)

  protected def makeTyp[C[_] <: CmdBld[_], B](typing: Typing[B])(f: (Tree[Denot], Typer[B]) => C[B]): C[B] = f(Tree.apply(typing), coerce(typing.proof))

  protected def makeTId[C[_] <: CmdBld[_], B](tid: TypedIdentifier[B])(f: (Tree[Denot], Typer[B]) => C[B]): C[B] = {
    val newProof = Read (tid.proof.show) { s =>
      tid.symbol
        .find(value => s startsWith value)
        .map(label => s drop label.value.length)
        .map(tid.proof.apply)
        .getOrElse(failure(s"Could not prove that the value of ${tid.symbol.show} is of type ${tid.proof.show}"))
    }

    f(Tree(tid.copy(proof = newProof)), coerce(newProof))
  }
}

private[core]
class Cmd[+A](val syntax: Tree[Denot], program: Typer[A]) extends CmdBld[A] {
  def run(args: List[String]): Result[A] = program.run(args)._2
}

private[core]
class Cmd0(syntax0: Tree[Denot]) extends CmdBld[Nothing] {
  def -(id: Identifier): Cmd0 = makeId[({type f[x] = Cmd0})#f, Nothing](id) { syntax => new Cmd0(syntax0 affix syntax) }

  def -[A](typ: Typing[A]): Cmd1[A] = makeTyp(typ) { (syntax, types) => new Cmd1(syntax0 infix syntax, types) }

  def -[A](tid: TypedIdentifier[A]): Cmd1[A] = makeTId(tid) { (syntax, types) => new Cmd1(syntax0 affix syntax, types) }

  def apply[A](f: () => A): Cmd[A] = new Cmd(syntax0, coerce(_ => success(f())))

  class Cmd1[A](syntax1: Tree[Denot], types1: Typer[A]) extends CmdBld[A] {
    def -(id: Identifier): Cmd1[A] = makeId(id) { syntax => new Cmd1(syntax1 affix syntax, types1) }

    def -[B](typ: Typing[B]): Cmd2[B] = makeTyp(typ) { (syntax, types) => new Cmd2(syntax1 infix syntax, types) }

    def -[B](tid: TypedIdentifier[B]): Cmd2[B] = makeTId(tid) { (syntax, types) => new Cmd2(syntax1 affix syntax, types) }

    def apply[B](f: A => B): Cmd[B] = new Cmd(syntax1, applier.map(types1)(f))

    class Cmd2[B](syntax2: Tree[Denot], types2: Typer[B]) extends CmdBld[B] {
      def -(id: Identifier): Cmd2[B] = makeId(id) { syntax => new Cmd2(syntax2 affix syntax, types2) }

      def -[C](typ: Typing[C]): Cmd3[C] = makeTyp(typ) { (syntax, types) => new Cmd3(syntax2 infix syntax, types) }

      def -[C](tid: TypedIdentifier[C]): Cmd3[C] = makeTId(tid) { (syntax, types) => new Cmd3(syntax2 affix syntax, types) }

      def apply[C](f: (A, B) => C): Cmd[C] = new Cmd(syntax2, applier.apply2(types1, types2)(f))

      class Cmd3[C](syntax3: Tree[Denot], types3: Typer[C]) extends CmdBld[C] {
        def -(id: Identifier): Cmd3[C] = makeId(id) { syntax => new Cmd3(syntax3 affix syntax, types3) }

        def -[D](typ: Typing[D]): Cmd4[D] = makeTyp(typ) { (syntax, types) => new Cmd4(syntax3 infix syntax, types) }

        def -[D](tid: TypedIdentifier[D]): Cmd4[D] = makeTId(tid) { (syntax, types) => new Cmd4(syntax3 affix syntax, types) }

        def apply[D](f: (A, B, C) => D): Cmd[D] = new Cmd(syntax3, applier.apply3(types1, types2, types3)(f))

        class Cmd4[D](syntax4: Tree[Denot], types4: Typer[D]) extends CmdBld[D] {
          def -(id: Identifier): Cmd4[D] = makeId(id) { syntax => new Cmd4(syntax4 affix syntax, types4) }

          def -[E](typ: Typing[E]): Cmd5[E] = makeTyp(typ) { (syntax, types) => new Cmd5(syntax4 infix syntax, types) }

          def -[E](tid: TypedIdentifier[E]): Cmd5[E] = makeTId(tid) { (syntax, types) => new Cmd5(syntax4 affix syntax, types) }

          def apply[E](f: (A, B, C, D) => E): Cmd[E] = new Cmd(syntax4, applier.apply4(types1, types2, types3, types4)(f))

          class Cmd5[E](syntax5: Tree[Denot], types5: Typer[E]) extends CmdBld[E] {
            def -(id: Identifier): Cmd5[E] = makeId(id) { syntax => new Cmd5(syntax5 affix syntax, types5) }

            def -[F](typ: Typing[F]): Cmd6[F] = makeTyp(typ) { (syntax, types) => new Cmd6(syntax5 infix syntax, types) }

            def -[F](tid: TypedIdentifier[F]): Cmd6[F] = makeTId(tid) { (syntax, types) => new Cmd6(syntax5 affix syntax, types) }

            def apply[F](f: (A, B, C, D, E) => F): Cmd[F] = new Cmd(syntax5, applier.apply5(types1, types2, types3, types4, types5)(f))

            class Cmd6[F](syntax6: Tree[Denot], types6: Typer[F]) extends CmdBld[F] {

              def -(id: Identifier): Cmd6[F] = makeId(id) { syntax => new Cmd6(syntax6 affix syntax, types6) }

              def -[G](typ: Typing[G]): Cmd7[G] = makeTyp(typ) { (syntax, types) => new Cmd7(syntax6 infix syntax, types) }

              def -[G](tid: TypedIdentifier[G]): Cmd7[G] = makeTId(tid) { (syntax, types) => new Cmd7(syntax6 affix syntax, types) }

              def apply[G](f: (A, B, C, D, E, F) => G): Cmd[G] = new Cmd(syntax6, applier.apply6(types1, types2, types3, types4, types5, types6)(f))

              class Cmd7[G](syntax7: Tree[Denot], types7: Typer[G]) extends CmdBld[G] {

                def -(id: Identifier): Cmd7[G] = makeId(id) { syntax => new Cmd7(syntax7 affix syntax, types7) }

                def -[H](typ: Typing[H]): Cmd8[H] = makeTyp(typ) { (syntax, types) => new Cmd8(syntax7 infix syntax, types) }

                def -[H](tid: TypedIdentifier[H]): Cmd8[H] = makeTId(tid) { (syntax, types) => new Cmd8(syntax7 affix syntax, types) }

                def apply[H](f: (A, B, C, D, E, F, G) => H): Cmd[H] = new Cmd(syntax7, applier.apply7(types1, types2, types3, types4, types5, types6, types7)(f))

                class Cmd8[H](syntax8: Tree[Denot], types8: Typer[H]) extends CmdBld[H] {

                  def -(id: Identifier): Cmd8[H] = makeId(id) { syntax => new Cmd8(syntax8 affix syntax, types8) }

                  def -[I](typ: Typing[I]): Cmd9[I] = makeTyp(typ) { (syntax, types) => new Cmd9(syntax8 infix syntax, types) }

                  def -[I](tid: TypedIdentifier[I]): Cmd9[I] = makeTId(tid) { (syntax, types) => new Cmd9(syntax8 affix syntax, types) }

                  def apply[I](f: (A, B, C, D, E, F, G, H) => I): Cmd[I] = new Cmd(syntax8, applier.apply8(types1, types2, types3, types4, types5, types6, types7, types8)(f))

                  class Cmd9[I](syntax9: Tree[Denot], types9: Typer[I]) extends CmdBld[I] {

                    def -(id: Identifier): Cmd9[I] = makeId(id) { syntax => new Cmd9(syntax9 affix syntax, types9) }

                    def -[J](typ: Typing[J]): Cmd10[J] = makeTyp(typ) { (syntax, types) => new Cmd10(syntax9 infix syntax, types) }

                    def -[J](tid: TypedIdentifier[J]): Cmd10[J] = makeTId(tid) { (syntax, types) => new Cmd10(syntax9 affix syntax, types) }

                    def apply[J](f: (A, B, C, D, E, F, G, H, I) => J): Cmd[J] = new Cmd(syntax9, applier.apply9(types1, types2, types3, types4, types5, types6, types7, types8, types9)(f))

                    class Cmd10[J](syntax10: Tree[Denot], types10: Typer[J]) extends CmdBld[J] {

                      def -(id: Identifier): Cmd10[J] = makeId(id) { syntax => new Cmd10(syntax10 affix syntax, types10) }

                      def -[K](typ: Typing[K]): Cmd11[K] = makeTyp(typ) { (syntax, types) => new Cmd11(syntax10 infix syntax, types) }

                      def -[K](tid: TypedIdentifier[K]): Cmd11[K] = makeTId(tid) { (syntax, types) => new Cmd11(syntax10 affix syntax, types) }

                      def apply[K](f: (A, B, C, D, E, F, G, H, I, J) => K): Cmd[K] = new Cmd(syntax10, applier.apply10(types1, types2, types3, types4, types5, types6, types7, types8, types9, types10)(f))

                      class Cmd11[K](syntax11: Tree[Denot], types11: Typer[K]) extends CmdBld[K] {

                        def -(id: Identifier): Cmd11[K] = makeId(id) { syntax => new Cmd11(syntax11 affix syntax, types11) }

                        def -[L](typ: Typing[L]): Cmd12[L] = makeTyp(typ) { (syntax, types) => new Cmd12(syntax11 infix syntax, types) }

                        def -[L](tid: TypedIdentifier[L]): Cmd12[L] = makeTId(tid) { (syntax, types) => new Cmd12(syntax11 affix syntax, types) }

                        def apply[L](f: (A, B, C, D, E, F, G, H, I, J, K) => L): Cmd[L] = new Cmd(syntax11, applier.apply11(types1, types2, types3, types4, types5, types6, types7, types8, types9, types10, types11)(f))

                        class Cmd12[L](syntax12: Tree[Denot], types12: Typer[L]) extends CmdBld[L] {

                          def apply[M](f: (A, B, C, D, E, F, G, H, I, J, K, L) => M): Cmd[M] = new Cmd(syntax12, applier.apply12(types1, types2, types3, types4, types5, types6, types7, types8, types9, types10, types11, types12)(f))
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

  }

}
