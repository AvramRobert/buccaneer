package core

import Formatter.{Formatter, Lexical, lift}
import Formatter.syntax._
import Binary.treeSyntax
import core.Store._
import scalaz.syntax.traverse._
import scala.annotation.tailrec
import scalaz.{Applicative, Reader, Traverse}


object Man {

  case class HelpConfig(textWidth: Int, indentation: Int, columnSpacing: Int)

  type Section[A] = Reader[HelpConfig, A]

  implicit val traverseVector: Traverse[Vector] = new Traverse[Vector] {
    override def traverseImpl[G[_], A, B](fa: Vector[A])(f: (A) => G[B])(implicit ap: Applicative[G]): G[Vector[B]] = {
      fa.foldLeft(ap.point(Vector[B]())) { (gvb, a) =>
        ap.apply2(gvb, f(a))(_ :+ _)
      }
    }
  }

  implicit val lexicalChar: Lexical[Char] = new Lexical[Char] {
    override def blank = ' '

    override def break = '\n'

    override def continuation = '-'

    override def eq(a1: Char, a2: Char) = a1 == a2
  }

  def section[A](f: HelpConfig => A): Section[A] = Reader(f)

  def emptySection: Section[Formatter[Char]] = section(_ => Formatter.empty[Char])

  def text(s: String): Formatter[Char] = lift(s.toCharArray.toVector)

  def line(txt: String)(fsize: HelpConfig => Int): Section[Formatter[Char]] = section { config =>
    text(txt).
      push(config.indentation).
      every.
      ofWidth(config.indentation + fsize(config)).
      fillAll
  }

  def columned(left: String, right: String, largest: Int): Section[Formatter[Char]] = for {
    key <- line(left)(config => largest + config.indentation)
    value <- line(right)(config => config.textWidth - config.columnSpacing - config.indentation - largest)
    aligned <- section { config => key.align(value, config.columnSpacing) }
  } yield aligned

  def paired(command: Tree[Denot]): Vector[(String, String)] = {
    @tailrec def go(cur: Tree[Denot], acc: Vector[(String, String)] = Vector()): Vector[(String, String)] = cur match {
      case a -< (l, r) =>
        val left = l.foldLeft(a.show) { (str, denot) => s"$str ${denot.show}" }
        go(r, acc :+ (left, a.docs.msg))
      case Leaf => acc
    }

    go(command)
  }

  def command(tree: Tree[Denot]): Section[Formatter[Char]] = tree.
    takeWhile(_.isMajorIdentifier).
    serialise match {
    case name if name.isEmpty => emptySection
    case name => Reader { help =>
      text(name.
        map(_.show).
        mkString(" ") ++ s" - ${name.last.docs.msg}").
        ofWidth(help.textWidth).
        push(help.indentation).
        every
    }
  }

  def largest(all: Vector[Tree[Denot]]): Int = all.
    map {
      case a -< (l, _) => l.foldLeft(a.show.length)((x, y) => x + y.show.length) + 1
      case Leaf => 0
    }.
    max

  def usage(all: Vector[Tree[Denot]]): Section[Vector[Formatter[Char]]] = section { config =>
    val com = all.headOption.fold("")(_.takeWhile(_.isMajorIdentifier).string(" ")(_.show))

    Stream.continually(text(s"$com")).
      take(all.size).
      zip(all.map { a => text(a.dropWhile(_.isMajorIdentifier).string(" ")(_.show)) }).
      map {
        case (left, right) =>
          (left absorbT right).
            coeval.
            push(config.indentation - 1).
            ofWidth(config.textWidth)
      }.
      toVector
  }

  def subcommands(all: Vector[Tree[Denot]]): Section[Vector[Formatter[Char]]] = {
    val max = largest(all)
    all.filter(_.rootOf(_.isMajorIdentifier)).
      map(_.rootOption).
      distinct.
      map(_.fold(emptySection) { denot =>
        columned(denot.show, denot.docs.msg, max)
      }).
      sequenceU
  }

  def options(all: Vector[Tree[Denot]]): Section[Vector[Formatter[Char]]] = {
    val max = largest(all)
    all.filter(_.rootOf(denot => !denot.isMajorIdentifier)).
      flatMap(paired).
      distinct.
      map(t => columned(t._1, t._2, max)).
      sequenceU
  }

  def makeText(formatters: TraversableOnce[Formatter[Char]]): String = formatters.foldLeft(Vector.empty[Char]) { (acc, frmt) => acc ++ frmt.run }.mkString("")

  def whenEmpty(v: Vector[Formatter[Char]])(txt: => String): Vector[Formatter[Char]] = {
    if (v.isEmpty) Vector(text(txt))
    else v
  }

  def help[A](store: Store[MapT, A], input: List[String]): Section[String] = for {
    matched <- section(_ => Interpreter.partialMatch(store.keySet, input).toVector)
    zipped = matched.map(_ zips input)
    first = zipped.map(_.takeWhile(_._2.isDefined).map(_._1))
    rest = zipped.map(_.dropWhile(_._2.isDefined).map(_._1))
    commandSection <- first.headOption.fold(emptySection)(command)
    usageSection <- usage(matched)
    optionsSection <- options(rest)
    subcommandSection <- subcommands(rest)
    linebreak = Formatter.empty[Char]
  } yield makeText {
    (text("NAME") +:
      commandSection +:
      linebreak +:
      text("USAGE") +:
      whenEmpty(usageSection)("No usage information available.")) ++
      (linebreak +:
        text("OPTIONS") +:
        whenEmpty(optionsSection)("There are no options available.")) ++
      (linebreak +:
        text("COMMANDS") +:
        whenEmpty(subcommandSection)("There are no commands available.") :+
        linebreak :+
        text("Hint: You can call `--sgst` at any point to receive a list of all possible commands that match your current input."))
  }

  def suggest[A](store: Store[MapT, A], input: List[String]): Section[String] = section { _ =>
    makeText {
      Interpreter.partialMatch(store.keySet, input).
        map { tree =>
          text((tree zips input).string(" ") {
            case (_, Some(v)) => v
            case (denot, _) => denot.show
          })
        }
    }
  }

  def helper[A](store: Store[MapT, A], helpConfig: HelpConfig): List[String] => String = input => {
    help(store, input).run(helpConfig)
  }

  def suggester[A](store: Store[MapT, A], helpConfig: HelpConfig): List[String] => String = input => {
    suggest(store, input).run(helpConfig)
  }
}

object Formatter {
  self =>

  /*
  Notes for improvements:
    Idea: Define a formatter in terms what can be done at each line.
     One formatter might apply some formatting to each line, to some subset of lines or to a specific line.
     There are, however, some additional things it might do.
     It might adjust input in order to format something relative to something else. (i.e. filling with blanks, or hyphenating)
     This means that the formatting of some line might be adding additional lines to it.
     Hyphenation in particular is a special case of formatting that is applied at every line in relation to the previous one.
     This is where context comes into play and a monad might emerge.
     A monad can be defined by relating one homologous formatting to another. The homology in this scenario is defined
     in terms of the line at which the formatting occurs. `bind` might describe taking the line, formatting it and
     then using the formatted line to create another formatter that formats it some other way.
     OR: it can be used to relate lines together. For example, given some sub-sequence `F[A]` representing
     a previous line, i can `flatMap` it to create another formatter that formats the `NEXT` line in relation
     that one.
   */

  trait Lexical[A] {
    def blank: A

    def break: A

    def continuation: A

    def eq(a1: A, a2: A): Boolean

    def isBlank(a: A): Boolean = eq(a, blank)

    def isCont(a: A): Boolean = eq(a, continuation)
  }

  type VRead[A] = Vector[A] => Vector[A]

  sealed trait Card

  case object All extends Card

  case class Few(n: Int) extends Card

  //case class Exact(lines: List[Int]) extends Card

  sealed trait Formatter[A] {
    protected def data: Vector[A]

    def width: Int

    def breadth: Int = data.size

    def totalLines: Int = Math.ceil(breadth.toDouble / width.toDouble).toInt
  }

  case class Every[A](data: Vector[A], f: Vector[A] => Vector[A], width: Int) extends Formatter[A]

  case class More[A](data: Vector[A], f: Vector[A] => Vector[A], width: Int, at: Int, n: Card) extends Formatter[A]


  def blank[A](implicit lexical: Lexical[A]): A = lexical.blank

  def break[A](implicit lexical: Lexical[A]): A = lexical.break

  def continuation[A](implicit lexical: Lexical[A]): A = lexical.continuation

  def isBlank[A](a: A)(implicit lexical: Lexical[A]): Boolean = lexical.isBlank(a)

  def isCont[A](a: A)(implicit lexical: Lexical[A]): Boolean = lexical.isCont(a)

  def lift[A](data: Vector[A]): Formatter[A] = formatter(data)

  def formatter[A](data: Vector[A]): Formatter[A] = More(data, identity, data.size, 0, Few(1))

  def fold[A, B](formatter: Formatter[A])(f: Every[A] => B)(g: More[A] => B): B = formatter match {
    case every@Every(_, _, _) => f(every)
    case more@More(_, _, _, _, _) => g(more)
  }

  def endo[A](formatter: Formatter[A])(f: VRead[A] => VRead[A]): Formatter[A] = fold[A, Formatter[A]](formatter)(e => e.copy(f = f(e.f)))(m => m.copy(f = f(m.f)))

  def continue[A](formatter: Formatter[A])(f: Vector[A] => Vector[A]): Formatter[A] = endo(formatter)(_ andThen f)

  def prepend[A](formatter: Formatter[A], a: A): Formatter[A] = continue(formatter)(v => a +: v)

  def append[A](formatter: Formatter[A], a: A): Formatter[A] = continue(formatter)(_ :+ a)

  def every[A](formatter: Formatter[A]): Formatter[A] = fold(formatter)(identity)(x => Every(x.data, x.f, x.width))

  def one[A](formatter: Formatter[A]): Formatter[A] = fold(formatter)(e => More(e.data, e.f, e.width, 0, Few(1)))(m => More(m.data, m.f, m.width, m.at, Few(1)))

  def repeat[A](formatter: Formatter[A], amount: Int): Formatter[A] = {
    @tailrec def go(v: Vector[A], f: Vector[A] => Vector[A], n: Int): Vector[A] = {
      if (n <= 0) v
      else go(f(v), f, n - 1)
    }

    endo(formatter)(f => (v: Vector[A]) => go(v, f, amount))
  }

  def widen[A](bla: Formatter[A])(f: Int => Int): Formatter[A] = fold[A, Formatter[A]](bla)(e => e.copy(width = f(e.width)))(m => m.copy(width = f(m.width)))

  def assimilate[A](formatter: Formatter[A], data: Vector[A]): Formatter[A] = continue(formatter)(_ ++ data)

  def fill[A: Lexical](formatter: Formatter[A], n: Int): Formatter[A] = {
    val (data, width) = formatter.evaluate
    lift(data ++ (0 until n).map(_ => blank).toVector).ofWidth(width)
  }


  @tailrec def fillAll[A: Lexical](formatter: Formatter[A]): Formatter[A] = {
    val n = formatter.breadth % formatter.width
    if (n != 0) fillAll(fill(formatter, 1))
    else formatter
  }

  @tailrec def evaluate[A: Lexical](formatter: Formatter[A]): (Vector[A], Int) = formatter match {
    case Every(data, f, width) => evaluate(More(data, f, width, 0, All))
    case More(data, f, width, at, All) if at < data.size => evaluate(More(data, f, width, 0, Few(formatter.totalLines)))
    case More(data, f, width, at, Few(i)) if at < data.size && i > 0 =>
      val (start, end) = data.splitAt(at)
      evaluate(More(start ++ consume(end, width)(f), f, width, at + width, Few(i - 1)))
    case More(data, _, width, _, _) => (data, width)
  }

  def evaluateH[A: Lexical](formatter: Formatter[A]): (Vector[A], Int) = {
    val (data, width) = evaluate(formatter)
    (hyphenate(data, width), width)
  }

  def push[A: Lexical](formatter: Formatter[A], n: Int): Formatter[A] = formatter.prepend(blank).repeat(n)

  def absorb[A: Lexical](absorber: Formatter[A], absorbee: Formatter[A]): Formatter[A] = {
    val max = List(absorber.width, absorbee.width).max
    absorber
      .coeval
      .assimilate(
        absorbee.evaluate._1)
      .ofWidth(max)
  }

  def absorbT[A: Lexical](absorber: Formatter[A], absorbee: Formatter[A]): Formatter[A] = absorb(absorber, absorbee).ofWidth(absorber.width + absorbee.width)

  // This should describe what needs to happen, not actually do it
  def interleave[A: Lexical](f1: Formatter[A], f2: Formatter[A]): Formatter[A] = {
    val (left, lwidth) = f1.evaluate
    val (right, rwidth) = f2.evaluate

    lift(left.grouped(lwidth)
      .zip(right.grouped(rwidth))
      .flatMap(v => v._1 ++ v._2)
      .toVector)
      .widen(_ => lwidth + rwidth)
  }

  def empty[A: Lexical]: Formatter[A] = emptyN(1)

  def emptyN[A: Lexical](n: Int): Formatter[A] = {
    if (n <= 0) lift(Vector.empty[A])
    lift(Vector(blank)).prepend(blank).repeat(n - 1)
  }

  def normalise[A: Lexical](f1: Formatter[A], f2: Formatter[A]): (Formatter[A], Formatter[A]) = {
    def pad(larger: Formatter[A], smaller: Formatter[A]) = {
      val amount = (larger.totalLines * smaller.width) - smaller.breadth
      (larger.fillAll, smaller.fill(amount))
    }

    if (f1.totalLines >= f2.totalLines) pad(f1, f2)
    else pad(f2, f1).swap
  }

  def align[A: Lexical](f1: Formatter[A], f2: Formatter[A], distance: Int): Formatter[A] = {
    val (nf1, nf2) = normalise(f1, f2)
    val empty = emptyN(nf1.totalLines * distance).ofWidth(distance)
    nf1.coevalH interleave empty interleave nf2.coevalH
  }

  def run[A: Lexical](formatter: Formatter[A]): Vector[A] = evaluateH(formatter) match {
    case (data, width) => format(data, width)
  }

  def consume[A: Lexical](v: Vector[A], width: Int)(f: VRead[A]): Vector[A] = f(v.take(width)) ++ v.drop(width)

  /*
   // Almost works properly.. if however something like the following occurs:
        Hello my n
        ame is Robert.

        That becomes this:

        Hello my -
        name is Robert
   */
  def hyphenate[A: Lexical](v: Vector[A], width: Int) = {
    @tailrec def go(hyphenated: Vector[A], rem: Vector[A]): Vector[A] = rem match {
      case _ if rem.isEmpty => hyphenated
      case _ if hyphenated.isEmpty => go(rem.take(width), rem.drop(width))
      case _ =>
        val current = rem.take(width)
        val n = if (current.size < width) 0 else 1
        if (!isBlank(hyphenated.last) && !isCont(hyphenated.last) && !isBlank(current.head)) {
          go(hyphenated
            .dropRight(1)
            .:+(continuation)
            .++(hyphenated.last +: current)
            .dropRight(n), rem.drop(width - n))
        }
        else go(hyphenated ++ current, rem.drop(width))
    }

    go(Vector(), v)
  }

  def format[A: Lexical](input: Vector[A], width: Int): Vector[A] = {
    input
      .grouped(width)
      .flatMap(_ :+ break)
      .toVector
  }

  object syntax {

    implicit class FormatterSyntax[A: Lexical](formatter: Formatter[A]) {
      def continue(f: Vector[A] => Vector[A]): Formatter[A] = self.continue(formatter)(f)

      def every: Formatter[A] = self.every(formatter)

      def one: Formatter[A] = self.one(formatter)

      def ofWidth(i: Int): Formatter[A] = self.widen(formatter)(_ => i)

      def widen(f: Int => Int): Formatter[A] = self.widen(formatter)(f)

      def assimilate(v: Vector[A]): Formatter[A] = self.assimilate(formatter, v)

      def align(that: Formatter[A], distance: Int): Formatter[A] = self.align(formatter, that, distance: Int)

      def repeat(n: Int): Formatter[A] = self.repeat(formatter, n)

      def append(a: A): Formatter[A] = self.append(formatter, a)

      def prepend(a: A): Formatter[A] = self.prepend(formatter, a)

      def fill(n: Int): Formatter[A] = self.fill(formatter, n)

      def fillAll: Formatter[A] = self.fillAll(formatter)

      def interleave(that: Formatter[A]): Formatter[A] = self.interleave(formatter, that)

      def absorb(that: Formatter[A]): Formatter[A] = self.absorb(formatter, that)

      def push(n: Int): Formatter[A] = self.push(formatter, n)

      def coeval: Formatter[A] = {
        val (data, width) = evaluate
        self.formatter(data).ofWidth(width)
      }

      def coevalH: Formatter[A] = {
        val (data, width) = evaluateH
        self.formatter(data).ofWidth(width)
      }

      def absorbT(absorbee: Formatter[A]): Formatter[A] = self.absorbT(formatter, absorbee)

      def evaluate: (Vector[A], Int) = self.evaluate(formatter)

      def evaluateH: (Vector[A], Int) = self.evaluateH(formatter)

      def run: Vector[A] = self.run(formatter)
    }

  }

}