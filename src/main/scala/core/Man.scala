package core

import Formatter.Formatter
import scalaz.syntax.traverse._
import scala.annotation.tailrec
import scalaz.Reader

trait ManOps {
  def helpConfig(textWidth: Int, indentation: Int, columnSpacing: Int): HelpConfig = HelpConfig(textWidth, indentation, columnSpacing)
}

case class HelpConfig(textWidth: Int, indentation: Int, columnSpacing: Int)

object Man {

  type Section[A] = Reader[HelpConfig, A]

  def section[A](f: HelpConfig => A): Section[A] = Reader(f)

  def emptySection: Section[Formatter[Char]] = section(_ => Formatter.empty[Char])

  def text(s: String): Formatter[Char] = Formatter(s.toCharArray.toVector)

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
    toVector match {
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
    }.max

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

  def help[A](input: List[String], corresponding: Set[Tree[Denot]]): Section[String] = for {
    matched <- section(_ => corresponding.toVector)
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

  def suggest[A](input: List[String], corresponding: Set[Tree[Denot]]): Section[String] = section { _ =>
    makeText {
      corresponding.
        map { tree =>
          text((tree zips input).string(" ") {
            case (_, Some(v)) => v
            case (denot, _) => denot.show
          })
        }
    }
  }
}

private[core]
trait Lexical[A] {
  def blank: A

  def break: A

  def continuation: A

  def eq(a1: A, a2: A): Boolean

  def isBlank(a: A): Boolean = eq(a, blank)

  def isCont(a: A): Boolean = eq(a, continuation)
}


/*
  Notes for improvements:
    Idea: Define a formatter in terms of what can be done at each line.
     One such formatter would have three specific cases, that would need handling:
       1. Apply some formatting at every line of the input.
       2. Apply some formatting at some subset of lines of the input.
       3. Apply some formatting at one specific line of the input.
     Additionally, there could also exist some transitive relation between formats. For example one line might be formatted in
     relation to the previous one. (this is for example the case in hyphenation)
     This would therefore imply some context dependency and might lead to the formation of a Monad.

     Possible monad definitions:
     a) A Monad may be formed by relating one homologous formatting to another. `bind` might describe taking the line, formatting it and
     then using the formatted line to create another formatter that formats it some other way.
     b) A Monad may be formed by relating lines together. For example, given some sub-sequence `F[A]` representing
     a previous line, i can `flatMap` it to create another formatter that formats the `NEXT` line in relation
     to that one.
   */
private[core]
object Formatter {

  sealed trait Cardinality

  case object All extends Cardinality

  case class Few(n: Int) extends Cardinality

  type Format[A] = Vector[A] => Vector[A]

  def apply[A: Lexical](data: Vector[A]): Formatter[A] = More(data, identity, data.size, 0, Few(1))

  def empty[A](implicit lexical: Lexical[A]): Formatter[A] = Formatter(Vector(lexical.blank))

  //case class Exact(lines: List[Int]) extends Card

  private[core]
  sealed trait Formatter[A] {
    protected def data: Vector[A]

    def width: Int

    def breadth: Int = data.size

    def totalLines: Int = Math.ceil(breadth.toDouble / width.toDouble).toInt


    def blank(implicit lexical: Lexical[A]): A = lexical.blank

    def break(implicit lexical: Lexical[A]): A = lexical.break

    def continuation(implicit lexical: Lexical[A]): A = lexical.continuation

    def isBlank(a: A)(implicit lexical: Lexical[A]): Boolean = lexical.isBlank(a)

    def isCont(a: A)(implicit lexical: Lexical[A]): Boolean = lexical.isCont(a)


    def fold[B](f: Every[A] => B)(g: More[A] => B): B = this match {
      case every@Every(_, _, _) => f(every)
      case more@More(_, _, _, _, _) => g(more)
    }

    def endo(f: Format[A] => Format[A]): Formatter[A] = fold[Formatter[A]](e => e.copy(f = f(e.f)))(m => m.copy(f = f(m.f)))

    def continue(f: Format[A]): Formatter[A] = endo(_ andThen f)

    def prepend(a: A): Formatter[A] = continue(v => a +: v)

    def append(a: A): Formatter[A] = continue(_ :+ a)

    def every: Formatter[A] = fold(identity)(x => Every(x.data, x.f, x.width))

    def one: Formatter[A] = fold(e => More(e.data, e.f, e.width, 0, Few(1)))(m => More(m.data, m.f, m.width, m.at, Few(1)))

    def repeat(amount: Int): Formatter[A] = {
      @tailrec def go(v: Vector[A], f: Format[A], n: Int): Vector[A] = {
        if (n <= 0) v
        else go(f(v), f, n - 1)
      }

      endo(f => (v: Vector[A]) => go(v, f, amount))
    }

    def widen(f: Int => Int): Formatter[A] = fold[Formatter[A]](e => e.copy(width = f(e.width)))(m => m.copy(width = f(m.width)))

    def assimilate(data: Vector[A]): Formatter[A] = continue(_ ++ data)

    def fill(n: Int)(implicit lexical: Lexical[A]): Formatter[A] = {
      val (data, width) = evaluate
      Formatter(data ++ (0 until n).map(_ => blank).toVector).ofWidth(width)
    }

    @tailrec final def fillAll(implicit lexical: Lexical[A]): Formatter[A] = {
      val n = breadth % width
      if (n != 0) fill(1).fillAll
      else this
    }

    @tailrec private def safeEval(formatter: Formatter[A])(implicit lexical: Lexical[A]): (Vector[A], Int) = formatter match {
      case Every(data, f, width) => safeEval(More(data, f, width, 0, All))
      case More(data, f, width, at, All) if at < data.size => safeEval(More(data, f, width, 0, Few(totalLines)))
      case More(data, f, width, at, Few(i)) if at < data.size && i > 0 =>
        val (start, end) = data.splitAt(at)
        safeEval(More(start ++ consume(end, width)(f), f, width, at + width, Few(i - 1)))
      case More(data, _, width, _, _) => (data, width)
    }

    def evaluate(implicit lexical: Lexical[A]): (Vector[A], Int) = safeEval(this)

    def evaluateH(implicit lexical: Lexical[A]): (Vector[A], Int) = {
      val (data, width) = evaluate
      (hyphenate(data, width), width)
    }

    def push(n: Int)(implicit lexical: Lexical[A]): Formatter[A] = prepend(blank).repeat(n)

    def absorb(absorbee: Formatter[A])(implicit lexical: Lexical[A]): Formatter[A] = {
      val max = List(width, absorbee.width).max
      coeval
        .assimilate(
          absorbee.evaluate._1)
        .ofWidth(max)
    }

    def absorbT(absorbee: Formatter[A])(implicit lexical: Lexical[A]): Formatter[A] = absorb(absorbee).ofWidth(width + absorbee.width)

    // This should describe what needs to happen, not actually do it
    def interleave(f2: Formatter[A])(implicit lexical: Lexical[A]): Formatter[A] = {
      val (left, lwidth) = evaluate
      val (right, rwidth) = f2.evaluate

      Formatter(left.grouped(lwidth)
        .zip(right.grouped(rwidth))
        .flatMap(v => v._1 ++ v._2)
        .toVector)
        .widen(_ => lwidth + rwidth)
    }

    def emptyN(n: Int)(implicit lexical: Lexical[A]): Formatter[A] = {
      if (n <= 0) Formatter.empty[A]
      Formatter(Vector(blank)).prepend(blank).repeat(n - 1)
    }

    def normalise(f2: Formatter[A])(implicit lexical: Lexical[A]): (Formatter[A], Formatter[A]) = {
      def pad(larger: Formatter[A], smaller: Formatter[A]) = {
        val amount = (larger.totalLines * smaller.width) - smaller.breadth
        (larger.fillAll, smaller.fill(amount))
      }

      if (totalLines >= f2.totalLines) pad(this, f2)
      else pad(f2, this).swap
    }

    def align(f2: Formatter[A], distance: Int)(implicit lexical: Lexical[A]): Formatter[A] = {
      val (nf1, nf2) = normalise(f2)
      val empty = emptyN(nf1.totalLines * distance).ofWidth(distance)
      nf1.coevalH interleave empty interleave nf2.coevalH
    }

    def ofWidth(i: Int): Formatter[A] = widen(_ => i)

    def run(implicit lexical: Lexical[A]): Vector[A] = evaluateH match {
      case (data, width) => format(data, width)
    }

    def consume(v: Vector[A], width: Int)(f: Format[A]): Vector[A] = f(v.take(width)) ++ v.drop(width)

    /*
     // Almost works properly.. if however something like the following occurs:
          Hello my n
          ame is Robert.

          That becomes this:

          Hello my -
          name is Robert
     */
    def hyphenate(v: Vector[A], width: Int)(implicit lexical: Lexical[A]) = {
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

    def coeval(implicit lexical: Lexical[A]): Formatter[A] = {
      val (data, width) = evaluate
      Formatter(data).ofWidth(width)
    }

    def coevalH(implicit lexical: Lexical[A]): Formatter[A] = {
      val (data, width) = evaluateH
      Formatter(data).ofWidth(width)
    }


    def format(input: Vector[A], width: Int)(implicit lexical: Lexical[A]): Vector[A] = {
      input
        .grouped(width)
        .flatMap(_ :+ break)
        .toVector
    }
  }

  private[core]
  case class Every[A](data: Vector[A], f: Format[A], width: Int) extends Formatter[A]

  private[core]
  case class More[A](data: Vector[A], f: Format[A], width: Int, at: Int, n: Cardinality) extends Formatter[A]

}