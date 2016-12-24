package core

import scala.annotation.tailrec

object Man {

}

object Formatter {
  self =>

  /* Rules:
     1. Lines are defined in terms of a maximal number of characters.
     2. Insertion of elements should be compositional. Structures of text should be constructed by composing primitives.
     3. This should be a Monad. -> what should bind then mean?
     4. It should theoretically be able to spit out any type of output. Be it strings, bytes or other data representations. (it is here to format data in some way)

     This should be treated similarly to how a Parser is treated. You define formatters that do some sort of formatting on some input text.
     Formatters cannot format some text incrementally by "consuming" it, because they act globally on the whole text. Parsers seek incrementally different patterns
     in inputs. That's why they can "consume" it. Formatters could be made similar, but their application is rather more global. They apply their rules to the complete
     text.

     Bind => apply this formatting and then apply the next. But this would invalidate the previous format.
     This is essentially an endofunction on some type A, that modifies the structure of A.

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

  // I may have found a problem. By intending every line a number >= than the width, then it will never terminate, because the process always adds a new line
  // and considers it in the formatting. This should be caught.
  def indentLeft[A: Lexical](formatter: Formatter[A], n: Int): Formatter[A] = repeat(prepend(formatter, blank), n)

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

  @tailrec def evaluate[A](formatter: Formatter[A]): (Vector[A], Int) = formatter match {
    case Every(data, f, width) => evaluate(More(data, f, width, 0, All))
    case More(data, f, width, at, All) if at < data.size =>
      val (start, end) = data.splitAt(at)
      evaluate(More(start ++ consume(end, width)(f), f, width, at + width, All))
    case More(data, f, width, at, Few(i)) if at < data.size && i > 0 =>
      val (start, end) = data.splitAt(at)
      evaluate(More(start ++ consume(end, width)(f), f, width, at + width, Few(i - 1)))
    case More(data, _, width, _, _) => (data, width)
  }

  def evaluate1[A](formatter: Formatter[A]): Vector[A] = evaluate(formatter)._1

  def interleave[A: Lexical](f1: Formatter[A], f2: Formatter[A]): Formatter[A] = {
    val (left, lwidth) = f1.evaluate
    val (right, rwidth) = f2.evaluate

    lift(left.grouped(lwidth)
      .zip(right.grouped(rwidth))
      .flatMap(v => v._1 ++ v._2)
      .toVector)
      .widen(_ => lwidth + rwidth)
  }

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

    nf1 interleave empty interleave nf2
  }

  def run[A: Lexical](formatter: Formatter[A]): Vector[A] = evaluate(formatter) match {
    case (data, width) => format(data, width)
  }

  def consume[A](v: Vector[A], width: Int)(f: VRead[A]): Vector[A] = f(v.take(width)) ++ v.drop(width)

  def format[A: Lexical](input: Vector[A], width: Int): Vector[A] = {
    input
      .grouped(width)
      .flatMap {
        _ :+ break
        //        s =>
        //        val (left, right) = (s.head, s.last)
        //        if (isBlank(left.last) || isBlank(right.head)) left :+ break
        //        else left :+ continuation :+ break
      }
      .toVector
  }

  implicit class FormatterSyntax[A: Lexical](formatter: Formatter[A]) {
    def continue(f: Vector[A] => Vector[A]): Formatter[A] = self.continue(formatter)(f)

    def every: Formatter[A] = self.every(formatter)

    def one: Formatter[A] = self.one(formatter)

    def indentLeft(n: Int): Formatter[A] = self.indentLeft(formatter, n)

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

    def corun: Formatter[A] = lift(self.run(formatter))

    def coeval: Formatter[A] = {
      val (data, width) = evaluate
      self.formatter(data).ofWidth(width)
    }

    def evaluate1: Vector[A] = self.evaluate1(formatter)

    def evaluate: (Vector[A], Int) = self.evaluate(formatter)

    def run: Vector[A] = self.run(formatter)
  }

}

//import core.Texer.Text
//import Binary.treeSyntax
//
//object Man {
//
//  /**
//    * For columns and rows, this gets a little bit more difficult.
//    * I need to provide functions, which describe the
//    * shape of the formatting, that I intend to use.
//    * I then proceed to adapt the supplied text to this format by running the composition.
//    *
//    * Direct transformations on the text itself will not work that easily,
//    * because formatting, in this case, flows from one text to the next.
//    * I should be able to get away with it if I don't allow continuous text
//    * manipulation. In that case, I can define the overall structure
//    * of subsections of text and then proceed to combine them together
//    * individually.
//    *
//    * The proper behaviour of this should've been something similar
//    * to a descriptor of format. One defines the complete textual format
//    * one wants and then feeds some text to this. This then proceeds
//    * to apply the desired format to the text.
//    *
//    * I can however work on this in the next few days. This is
//    * a rough draft.
//    *
//    * // Currently, columns can be made like so
//    *
//    * val opts = List("-r", "-a", "-c") map instance.lift
//    * val descs = List("Desc for `-r`", "Desc for `-a`", "Desc for `-c`") map instance.lift
//    *
//    * (opts zip descs)
//    * .map {
//    * case (opt, desc) => List(opt).indent(2).append(List(desc).spacing(1).indent(5)).line
//    * }
//    * .fold(_ append _).make
//    *
//    * => creates expected
//    * The ugly part of this is that I have to lift simple tokens into a list, in order to receive the `Text[_]` context
//    * Some of these operations might also abstract to simpler combinators, which I can more transparently compose together
//    * to create the columns. This should definitely be a priority.
//    * For a rough version of this API, I think `line`, `indent`, `append`, `prepend` and `spacing` are sufficiently
//    * powerful primitives for describing simple Man pages. After adding some more combinators for simplification,
//    * this could do the trick for the first versions. This should however be improved upon.
//    * There are a lot of other things that this should be able to do. One of the most important ones is laziness!
//    *
//    * === Man Page layout ===
//    * -> Name
//    *     - command name and short description
//    * -> Synopsis
//    *     - formal description of command usage
//    * -> Description
//    *     - textual description of the command function itself
//    * -> [Options]
//    *     - command options and their descriptions
//    * -> Examples
//    *     - examples of how to use the command
//    * -> See also
//    *     - commands related to this command
//    *
//    * => I currently can provide name, a synopsis and options
//    * I sadly do not support additional information such as a description
//    **/
//
//  // TODO: This has to be rewritten. I do not like it at all
//  import Texer._
//  import Texer.syntax._
//
//  val spacing = 1
//  val colIndent = 7
//  val colAlign = 10
//  val lineSpace = 1
//
//  def liftString(text: String): Text[String] = (text split " " toList) map instance.lift
//
//  def row(items: Text[String]*)(f: Text[String] => Text[String]): Text[String] = {
//    items
//      .zipWithIndex
//      .reduce { (l, r) =>
//        (l._1.relIndent(f(r._1.spacing(spacing)))(colAlign * r._2), l._2)
//      }
//      ._1
//      .indent(colIndent)
//      .line
//  }
//
//  def simpleRow(items: Text[String]*): Text[String] = row(items: _*)(identity)
//
//  def options(all: List[Tree[Sym]]): Text[String] = {
//    all.flatMap {
//      _
//        .filterL(com => com.isNamed || com.isAssigned || com.isAlt)
//        .flatMap {
//          case Named(l, d) => simpleRow(liftString(l), liftString(d.description)).line
//          case Assign(l, d) => simpleRow(liftString(l), liftString(d.description)).line
//          case Alt(ths, tht, d) => simpleRow(liftString(s"$ths, $tht"), liftString(d.description)).line
//          case _ => Nil
//        }
//    }
//  }
//
//  def cmd(all: List[Tree[Sym]]): Text[String] = {
//    all
//      .filter(_.rootOf(_.isCommand))
//      .map(_.rootOption.get)
//      .distinct
//      .map {
//        case Com(a, d) => liftString(a)
//        case _ => Nil
//      }
//      .head
//  }
//
//  def synopsis(all: List[Tree[Sym]]): Text[String] = {
//    val init = cmd(all)
//    val sentinel = init map (_ => tex.blank)
//    val sentients = init :: (0 until (all.size - 1)).map(_ => sentinel).toList
//    (sentients zip all) flatMap {
//      case (sent, tree) =>
//        row(sent,
//          tree.foldLeft(List.empty[Token[String]]) {
//            case (lst, Com(l, _)) => lst
//            case (lst, Named(l, _)) => lst append liftString(l)
//            case (lst, Assign(l, _)) => lst append liftString(s"$l<param>")
//            case (lst, Type(_)) => lst append liftString("<param>")
//            case (lst, _) => lst
//          })(_
//          .prepend(liftString("["))
//          .append(liftString("]")))
//    }
//  }
//
//  def name(all: List[Tree[Sym]]): Text[String] = {
//    all
//      .filter(_.rootOf(_.isCommand))
//      .map(_.rootOption.get)
//      .distinct
//      .flatMap {
//        case Com(a, d) => (liftString(a) append liftString(s" - ${d.description}").spacing(spacing)).indent(colIndent)
//        case _ => Nil
//      }
//      .line
//  }
//
//  def title(s: String): Text[String] = liftString(s).hspace(lineSpace)
//
//  def build(all: List[Tree[Sym]]): String =
//    title("NAME")
//      .append(name(all))
//      .hspace(lineSpace)
//      .append(title("SYNOPSIS"))
//      .append(synopsis(all))
//      .hspace(lineSpace)
//      // .append(title("DESCRIPTION"))
//      // .hspace(lineSpace)
//      .append(title("OPTIONS"))
//      .append(options(all))
//      .make
//
//  def buildFor(all: List[Tree[Sym]], com: Sym): String = build(all.filter(_.rootOf(_ == com)))
//
//}
//
//
//sealed trait Token[+A] {
//  def isBlank: Boolean = this match {
//    case BlankToken => true
//    case _ => false
//  }
//
//  def isNewLine: Boolean = this match {
//    case NewLineToken => true
//    case _ => false
//  }
//}
//
//case class WordToken[A](word: A) extends Token[A]
//
//case object BlankToken extends Token[Nothing]
//
//case object NewLineToken extends Token[Nothing]
//
//object Texer {
//  type Text[A] = List[Token[A]]
//
//  val instance = new Texer[String] {
//    override def zero: String = ""
//
//    override def size(t: String): Int = t.length
//
//    override def empty: String = " "
//
//    override def newline: String = "\n"
//
//    override def plus(a1: String, a2: String): String = a1 + a2
//  }
//
//  val syntax = TexerSyntax(instance)
//
//}
//
//trait Texer[A] {
//  self =>
//
//  val blank = BlankToken
//  val newln = NewLineToken
//
//  def append(ts: Text[A], tht: Text[A]): Text[A] = tht ++ ts //this should shave any empty things from the right
//
//  def append(tk: Token[A], tht: Text[A]): Text[A] = tht :+ tk
//
//  def prepend(ts: Text[A], tht: Text[A]): Text[A] = ts ++ tht //this should shave any empty things from the left
//
//  def prepend(tk: Token[A], tht: Text[A]): Text[A] = tk :: tht
//
//  def lift(item: A): Token[A] = WordToken(item)
//
//  def shaveLeft(text: Text[A])(amount: Int): Text[A] = text drop amount
//
//  def shaveRight(text: Text[A])(amount: Int): Text[A] = text dropRight amount
//
//  def manyOf(t: Token[A], i: Int): Text[A] = Stream.continually(t) take i toList
//
//  //a law: tex.shaveL(tex.indent(_)(n))(_.isBlank) == _
//  def shaveL(t: Text[A])(p: Token[A] => Boolean): Text[A] = t dropWhile p
//
//  def shaveR(t: Text[A])(p: Token[A] => Boolean): Text[A] = t.reverse dropWhile p reverse //this should be implemented better
//
//  def intersperse(t: Text[A])(tk: Token[A], interval: Int): Text[A] = partitioned(t)(interval).flatMap(l => append(tk, l))
//
//  def intersperse2(t: Text[A])(tks: Text[A], interval: Int): Text[A] = partitioned(t)(interval).flatMap(t => append(tks, t))
//
//  def colift(t: Token[A]): A = t match {
//    case WordToken(a) => a
//    case NewLineToken => newline
//    case BlankToken => empty
//  }
//
//  def partitioned(t: Text[A])(at: Int): List[Text[A]] = t grouped at toList
//
//  def indent(t: Text[A])(i: Int): Text[A] = prepend(manyOf(blank, i), t)
//
//  def shaveBoth(t: Text[A])(p: Token[A] => Boolean): Text[A] = shaveR(shaveL(t)(p))(p)
//
//  def length(t: Text[A]): Int = t.foldLeft(0)((a, b) => a + tokenSize(b))
//
//  def relIndent(ts: Text[A], tht: Text[A])(pos: Int): Text[A] = append(indent(tht)(pos - length(ts)), ts)
//
//  def spacing(t: Text[A])(i: Int): Text[A] = shaveBoth(intersperse2(t)(manyOf(blank, i), 1))(_.isBlank)
//
//  def line(t: Text[A]): Text[A] = append(newln, t)
//
//  def hspace(t: Text[A])(amount: Int): Text[A] = append(manyOf(newln, amount), t)
//
//  def liftT(t: A): Text[A] = List(lift(t))
//
//  //new lines should shave their left padding
//  def lines(t: Text[A])(every: Int): Text[A] = {
//    partitioned(t)(every)
//      .map(shaveL(_)(_.isBlank))
//      .flatMap(line)
//  }
//
//  def make(t: Text[A]): A = {
//    shaveR(t)(_.isNewLine)
//      .map(colift)
//      .fold(zero)((a1, a2) => plus(a1, a2))
//  }
//
//  def tokenSize(a: Token[A]): Int = size(colift(a))
//
//  def plus(a1: A, a2: A): A
//
//  def size(t: A): Int
//
//  def zero: A
//
//  def empty: A
//
//  def newline: A
//}
//
//case class TexerSyntax[A](tex: Texer[A]) {
//  implicit def ops(t: Text[A]): TexerOps[A] = new TexerOps[A](t)(tex)
//}
//
//final class TexerOps[A](t: Text[A])(implicit tex: Texer[A]) {
//  def append(ts: Text[A]): Text[A] = tex.append(ts, t)
//
//  def prepend(ts: Text[A]): Text[A] = tex.prepend(ts, t)
//
//  def lines(every: Int): Text[A] = tex.lines(t)(every)
//
//  def line: Text[A] = tex.line(t)
//
//  def shaveR(f: Token[A] => Boolean): Text[A] = tex.shaveR(t)(f)
//
//  def spacing(by: Int): Text[A] = tex.spacing(t)(by)
//
//  def indent(by: Int): Text[A] = tex.indent(t)(by)
//
//  def hspace(by: Int): Text[A] = tex.hspace(t)(by)
//
//  def relIndent(t2: Text[A])(pos: Int) = tex.relIndent(t, t2)(pos)
//
//  def make: A = tex.make(t)
//}
//
//
