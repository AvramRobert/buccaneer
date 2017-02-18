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

  def emptySection: Section[Formatter] = section(_ => Formatter.empty)

  def text(s: String): Formatter = Formatter(s.toCharArray.toVector)

  def line(txt: String)(fsize: HelpConfig => Int): Section[Formatter] = section { config =>
    text(txt).
      push(config.indentation).
      every.
      ofWidth(config.indentation + fsize(config)).
      fillAll
  }

  def columned(left: String, right: String, largest: Int): Section[Formatter] = for {
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

  def command(tree: Tree[Denot]): Section[Formatter] = tree.
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

  def usage(all: Vector[Tree[Denot]]): Section[Vector[Formatter]] = section { config =>
    val com = all.headOption.fold("")(_.takeWhile(_.isMajorIdentifier).string(" ")(_.show))

    Stream.continually(text(com)).
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

  def subcommands(all: Vector[Tree[Denot]]): Section[Vector[Formatter]] = {
    val max = largest(all)
    all.filter(_.rootOf(_.isMajorIdentifier)).
      map(_.rootOption).
      distinct.
      map(_.fold(emptySection) { denot =>
        columned(denot.show, denot.docs.msg, max)
      }).
      sequenceU
  }

  def options(all: Vector[Tree[Denot]]): Section[Vector[Formatter]] = {
    val max = largest(all)
    all.filter(_.rootOf(denot => !denot.isMajorIdentifier)).
      flatMap(paired).
      distinct.
      map(t => columned(t._1, t._2, max)).
      sequenceU
  }

  def makeText(formatters: TraversableOnce[Formatter]): String = formatters.foldLeft(Vector.empty[Char]) { (acc, frmt) => acc ++ frmt.runH }.mkString("")
  def makeText(f: Formatter): String = makeText(List(f))

  def whenEmpty(v: Vector[Formatter])(txt: => String): Vector[Formatter] = {
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
    linebreak = Formatter.empty
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
object Formatter {

  private[core]
  sealed trait Cardinality

  private[core]
  case object All extends Cardinality

  private[core]
  case class Few(n: Int) extends Cardinality

  type Format[A] = Vector[A] => Vector[A]

  def apply(data: Vector[Char]): Formatter = More(data, identity, data.size, 0, Few(1))

  def empty: Formatter = Formatter(Vector(' '))

  private[core]
  sealed trait Formatter {
    protected def data: Vector[Char]
    def width: Int

    def blank: Char = ' '

    def break: Char = '\n'

    def hyphen: Char = '-'

    def isBlank(c: Char): Boolean = c == blank

    def isHyphen(c: Char): Boolean = c == hyphen

    def breadth: Int = data.size

    def totalLines: Int = Math.ceil(breadth.toDouble / width.toDouble).toInt

    def fold[B](f: Every => B)(g: More => B): B = this match {
      case every@Every(_, _, _) => f(every)
      case more@More(_, _, _, _, _) => g(more)
    }

    def endo(f: Format[Char] => Format[Char]): Formatter = fold[Formatter](e => e.copy(f = f(e.f)))(m => m.copy(f = f(m.f)))

    def continue(f: Format[Char]): Formatter = endo(_ andThen f)

    def prepend(c: Char): Formatter = continue(v => c +: v)

    def append(c: Char): Formatter = continue(_ :+ c)

    def every: Formatter = fold(identity)(x => Every(x.data, x.f, x.width))

    def one: Formatter = fold(e => More(e.data, e.f, e.width, 0, Few(1)))(m => More(m.data, m.f, m.width, m.at, Few(1)))

    def repeat(amount: Int): Formatter = {
      @tailrec def go(v: Vector[Char], f: Format[Char], n: Int): Vector[Char] = {
        if (n <= 0) v
        else go(f(v), f, n - 1)
      }

      endo(f => (v: Vector[Char]) => go(v, f, amount))
    }

    def widen(f: Int => Int): Formatter = fold[Formatter](e => e.copy(width = f(e.width)))(m => m.copy(width = f(m.width)))

    def assimilate(data: Vector[Char]): Formatter = continue(_ ++ data)

    def fill(n: Int): Formatter = {
      val (data, width) = evaluate
      Formatter(data ++ (0 until n).map(_ => blank).toVector).ofWidth(width)
    }

    @tailrec final def fillAll: Formatter = {
      val n = breadth % width
      if (n != 0) fill(1).fillAll
      else this
    }

    def push(n: Int): Formatter = prepend(blank).repeat(n)

    def evaluate: (Vector[Char], Int) = this match {
      case Every(data, f, width) => More(data, f, width, 0, All).evaluate
      case More(data, f, width, at, All) if at < data.size => More(data, f, width, 0, Few(totalLines)).evaluate
      case More(data, f, width, at, Few(i)) if at < data.size && i > 0 =>
        val cursor = (data grouped width).toVector
        ((cursor.take(at) ++ cursor.slice(at, at + i).map(f) ++ cursor.drop(at + i)).flatten, width)
      case More(data, _, width, _, _) => (data, width)
    }

    def evaluateH: (Vector[Char], Int) = {
      val (data, width) = evaluate
      (hyphenate(data, width), width)
    }

    def absorb(absorbee: Formatter): Formatter = {
      val max = List(width, absorbee.width).max
      coeval
        .assimilate(
          absorbee.evaluate._1)
        .ofWidth(max)
    }

    def absorbT(absorbee: Formatter): Formatter = absorb(absorbee).ofWidth(width + absorbee.width)

    // This should describe what needs to happen, not actually do it
    def interleave(f2: Formatter): Formatter = {
      val (left, lwidth) = evaluate
      val (right, rwidth) = f2.evaluate

      Formatter(left.grouped(lwidth)
        .zip(right.grouped(rwidth))
        .flatMap(v => v._1 ++ v._2)
        .toVector)
        .widen(_ => lwidth + rwidth)
    }

    def emptyN(n: Int): Formatter = {
      if (n <= 0) Formatter.empty
      Formatter(Vector(blank)).prepend(blank).repeat(n - 1)
    }

    def normalise(f2: Formatter): (Formatter, Formatter) = {
      def pad(larger: Formatter, smaller: Formatter) = {
        val amount = (larger.totalLines * smaller.width) - smaller.breadth
        (larger.fillAll, smaller.fill(amount))
      }

      if (totalLines >= f2.totalLines) pad(this, f2)
      else pad(f2, this).swap
    }

    def align(f2: Formatter, distance: Int): Formatter = {
      val (nf1, nf2) = normalise(f2)
      val empty = emptyN(nf1.totalLines * distance).ofWidth(distance)
      nf1.coevalH interleave empty interleave nf2.coevalH
    }

    def ofWidth(i: Int): Formatter = widen(_ => i)

    def run: Vector[Char] = withLines.every.evaluate._1
    def runH: Vector[Char] = coevalH.run

    /*
     // Almost works properly.. if however something like the following occurs:
          Hello my n
          ame is Robert.

          That becomes this:

          Hello my -
          name is Robert
     */
    def hyphenate(v: Vector[Char], width: Int) = {
      @tailrec def go(hyphenated: Vector[Char], rem: Vector[Char]): Vector[Char] = rem match {
        case _ if rem.isEmpty => hyphenated
        case _ if hyphenated.isEmpty => go(rem.take(width), rem.drop(width))
        case _ =>
          val current = rem.take(width)
          val n = if (current.size < width) 0 else 1
          if (!isBlank(hyphenated.last) && !isHyphen(hyphenated.last) && !isBlank(current.head)) {
            go(hyphenated
              .dropRight(1)
              .:+(hyphen)
              .++(hyphenated.last +: current)
              .dropRight(n), rem.drop(width - n))
          }
          else go(hyphenated ++ current, rem.drop(width))
      }

      go(Vector(), v)
    }

    def coeval: Formatter = {
      val (data, width) = evaluate
      Formatter(data).ofWidth(width)
    }

    def coevalH: Formatter = {
      val (data, width) = evaluateH
      Formatter(data).ofWidth(width)
    }

    def withLines: Formatter = continue(_ :+ '\n')
  }

  private[core]
  case class Every(data: Vector[Char], f: Format[Char], width: Int) extends Formatter

  case class More(data: Vector[Char], f: Format[Char], width: Int, at: Int, n: Cardinality) extends Formatter
}