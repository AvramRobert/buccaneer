package core

import scala.util.{Failure, Success, Try}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scalaz.{Bind, Kleisli, ValidationNel, \/}
import scalaz.syntax.either._
import Validators._
import shapeless.HMap
import CMap._

object CMap {
  def typedMap[A] = TMap[A]()

  def untypedMap = UMap[Any]()

  sealed trait Get[M[_], A] {
    def get(m: M[A])(key: Inter[Sym]): Option[Runner[A]]
  }

  class Bond[K, +V]

  case class TMap[A](private val underlying: Map[Inter[Sym], Runner[A]] = Map[Inter[Sym], Runner[A]]()) {
    lazy val keySet: Set[Inter[Sym]] = underlying.keySet

    def +(r: Runner[A]): TMap[A] = TMap(underlying + (r.syntax -> r))

    def get(key: Inter[Sym]): Option[Runner[A]] = underlying get key
  }

  case class UMap[A](private val underlying: HMap[Bond] = HMap.empty[Bond], keySet: Set[Inter[Sym]] = Set()) {
    implicit def bond[V <: Any] = new Bond[Inter[Sym], V]

    def +[B](r: Runner[B]): UMap[A] = UMap[A](underlying + (r.syntax -> r), keySet + r.syntax)

    def get(key: Inter[Sym]): Option[Runner[A]] = underlying get[Inter[Sym], Runner[A]] key
  }

  implicit def getTMap[A]: Get[TMap, A] = new Get[TMap, A] {
    override def get(m: TMap[A])(key: Inter[Sym]): Option[Runner[A]] = m get key
  }

  implicit def getUMap: Get[UMap, Any] = new Get[UMap, Any] {
    override def get(m: UMap[Any])(key: Inter[Sym]): Option[Runner[Any]] = m get key
  }
}

//TODO: Can't I generalise this? Or should'nt I generalise this?
object Interpreter {
  type Slight[A] = ValidationNel[Throwable, A]
  type Step[A, B] = Kleisli[Slight, A, B]

  implicit val bindSlight: Bind[Slight] = new Bind[Slight] {
    override def bind[A, B](fa: Slight[A])(f: (A) => Slight[B]): Slight[B] = fa flatMap f

    override def map[A, B](fa: Slight[A])(f: (A) => B): Slight[B] = fa map f
  }

  def step[A, B](f: A => Slight[B]): Step[A, B] = Kleisli[Slight, A, B](f)

  def interpret[A](map: TMap[A]) = resolve(map.keySet) andThen run(map)(getTMap)

  def interpret(map: UMap[Any]) = resolve(map.keySet) andThen run(map)(getUMap)

  def resolve(keySet: Set[Inter[Sym]]) =
    shape(keySet) andThen
      validate(syntax) andThen
      validate(types) andThen
      narrow

  def shape(commands: Set[Inter[Sym]]) = step { (input: List[String]) =>
    input match {
      case l@h :: _ =>
        commands
          .filter {
            case Com(a) :~ t => a == h
            case _ => false
          }
          .filter(_.depth == l.size)
          .map(_ zipL l)
          .toList
          .successNel
      case Nil => Nil.successNel
    }
  }

  def validate(f: ((Sym, String)) => Throwable \/ (Sym, String)) = step { (is: List[Inter[(Sym, String)]]) =>
    lazy val point = List.empty[Inter[(Sym, String)]].successNel[Throwable]
    is
      .map(Inter.validate(_)(f))
      .filter(_.isSuccess)
      .foldRight(point)((a, b) => (a |@| b) (_ :: _))
  }

  def narrow = step { (rem: List[Inter[(Sym, String)]]) =>
    rem match {
      case h :: Nil => h.successNel
      case h :: t => new Throwable("Ambiguous command. Too many commands match the input").failureNel
      case Nil => new Throwable("Input is not a valid command").failureNel
    }
  }

  def run[M[_], A](m: M[A])(implicit G: Get[M, A]) = step { (command: Inter[(Sym, String)]) =>
    val key = command map (_._1)
    G.get(m)(key)
      .fold(new Throwable("Unknown command").failureNel[A]) { runner =>
        (normalise _ andThen runner.run) (command)
      }
  }
}

object Validators {
  def normalise(syntax: Inter[(Sym, String)]): List[String] = {
    syntax filterL (_._1 isTyped) map {
      case ((Assign(l, op), input)) => (input split op) (1)
      case (_, input) => input
    }
  }

  def syntax(assoc: (Sym, String)) = assoc._1 match {
    case Com(label) =>
      if (label == assoc._2) assoc.right
      else new Throwable(s"Input of `${assoc._2}` does not match expected `$label`").left
    case Named(label) =>
      if (label == assoc._2) assoc.right
      else new Throwable(s"Input of `${assoc._2}` does not match expected `$label`").left
    case Assign(label, op) =>
      if (assoc._2 startsWith (label + op)) assoc.right
      else new Throwable(s"Improper assignment for `$label`").left
    case _ => assoc.right
  }

  def types(assoc: (Sym, String)) = assoc._1 match {
    case Type(m) =>
      Try(m(assoc._2)) match {
        case Success(_) => assoc.right
        case Failure(_) => new Throwable(s"Could not prove type of `${assoc._2}`").left
      }
    case _ => assoc.right
  }
}

