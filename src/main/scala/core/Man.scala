package core

import core.Texer.Text
import Binary.treeSyntax

object Man {

  /**
    * For columns and rows, this gets a little bit more difficult.
    * I need to provide functions, which describe the
    * shape of the formatting, that I intend to use.
    * I then proceed to adapt the supplied text to this format by running the composition.
    *
    * Direct transformations on the text itself will not work that easily,
    * because formatting, in this case, flows from one text to the next.
    * I should be able to get away with it if I don't allow continuous text
    * manipulation. In that case, I can define the overall structure
    * of subsections of text and then proceed to combine them together
    * individually.
    *
    * The proper behaviour of this should've been something similar
    * to a descriptor of format. One defines the complete textual format
    * one wants and then feeds some text to this. This then proceeds
    * to apply the desired format to the text.
    *
    * I can however work on this in the next few days. This is
    * a rough draft.
    *
    * // Currently, columns can be made like so
    *
    * val opts = List("-r", "-a", "-c") map instance.lift
    * val descs = List("Desc for `-r`", "Desc for `-a`", "Desc for `-c`") map instance.lift
    *
    * (opts zip descs)
    * .map {
    * case (opt, desc) => List(opt).indent(2).append(List(desc).spacing(1).indent(5)).line
    * }
    * .fold(_ append _).make
    *
    * => creates expected
    * The ugly part of this is that I have to lift simple tokens into a list, in order to receive the `Text[_]` context
    * Some of these operations might also abstract to simpler combinators, which I can more transparently compose together
    * to create the columns. This should definitely be a priority.
    * For a rough version of this API, I think `line`, `indent`, `append`, `prepend` and `spacing` are sufficiently
    * powerful primitives for describing simple Man pages. After adding some more combinators for simplification,
    * this could do the trick for the first versions. This should however be improved upon.
    * There are a lot of other things that this should be able to do. One of the most important ones is laziness!
    *
    * === Man Page layout ===
    * -> Name
    *     - command name and short description
    * -> Synopsis
    *     - formal description of command usage
    * -> Description
    *     - textual description of the command function itself
    * -> [Options]
    *     - command options and their descriptions
    * -> Examples
    *     - examples of how to use the command
    * -> See also
    *     - commands related to this command
    *
    * => I currently can provide name, a synopsis and options
    * I sadly do not support additional information such as a description
    **/

  // TODO: This has to be rewritten. I do not like it at all
  import Texer._
  import Texer.syntax._

  val spacing = 1
  val colIndent = 7
  val colAlign = 10
  val lineSpace = 1

  def liftString(text: String): Text[String] = (text split " " toList) map instance.lift

  def row(items: Text[String]*)(f: Text[String] => Text[String]): Text[String] = {
    items
      .zipWithIndex
      .reduce { (l, r) =>
        (l._1.relIndent(f(r._1.spacing(spacing)))(colAlign * r._2), l._2)
      }
      ._1
      .indent(colIndent)
      .line
  }

  def simpleRow(items: Text[String]*): Text[String] = row(items: _*)(identity)

  def options(all: List[Tree[Sym]]): Text[String] = {
    all.flatMap {
      _
        .filterL(com => com.isNamed || com.isAssigned || com.isAlt)
        .flatMap {
          case Named(l, d) => simpleRow(liftString(l), liftString(d.description)).line
          case Assign(l, op, d) => simpleRow(liftString(l), liftString(d.description)).line
          case Alt(ths, tht, d) => simpleRow(liftString(s"$ths, $tht"), liftString(d.description)).line
          case _ => Nil
        }
    }
  }

  def cmd(all: List[Tree[Sym]]): Text[String] = {
    all
      .filter(_.rootOf(_.isCommand))
      .map(_.rootOption.get)
      .distinct
      .map {
        case Com(a, d) => liftString(a)
        case _ => Nil
      }
      .head
  }

  def synopsis(all: List[Tree[Sym]]): Text[String] = {
    val init = cmd(all)
    val sentinel = init map (_ => tex.blank)
    val sentients = init :: (0 until (all.size - 1)).map(_ => sentinel).toList
    (sentients zip all) flatMap {
      case (sent, tree) =>
        row(sent,
          tree.foldLeft(List.empty[Token[String]]) {
            case (lst, Com(l, _)) => lst
            case (lst, Named(l, _)) => lst append liftString(l)
            case (lst, Assign(l, op, _)) => lst append liftString(s"$l$op<param>")
            case (lst, Type(_)) => lst append liftString("<param>")
            case (lst, _) => lst
          })(_
          .prepend(liftString("["))
          .append(liftString("]")))
    }
  }

  def name(all: List[Tree[Sym]]): Text[String] = {
    all
      .filter(_.rootOf(_.isCommand))
      .map(_.rootOption.get)
      .distinct
      .flatMap {
        case Com(a, d) => (liftString(a) append liftString(s" - ${d.description}").spacing(spacing)).indent(colIndent)
        case _ => Nil
      }
      .line
  }

  def title(s: String): Text[String] = liftString(s).hspace(lineSpace)

  def build(all: List[Tree[Sym]]): String =
    title("NAME")
      .append(name(all))
      .hspace(lineSpace)
      .append(title("SYNOPSIS"))
      .append(synopsis(all))
      .hspace(lineSpace)
      // .append(title("DESCRIPTION"))
      // .hspace(lineSpace)
      .append(title("OPTIONS"))
      .append(options(all))
      .make

  def buildFor(all: List[Tree[Sym]], com: Sym): String = build(all.filter(_.rootOf(_ == com)))

}


sealed trait Token[+A] {
  def isBlank: Boolean = this match {
    case BlankToken => true
    case _ => false
  }

  def isNewLine: Boolean = this match {
    case NewLineToken => true
    case _ => false
  }
}

case class WordToken[A](word: A) extends Token[A]

case object BlankToken extends Token[Nothing]

case object NewLineToken extends Token[Nothing]

object Texer {
  type Text[A] = List[Token[A]]

  val instance = new Texer[String] {
    override def zero: String = ""

    override def size(t: String): Int = t.length

    override def empty: String = " "

    override def newline: String = "\n"

    override def plus(a1: String, a2: String): String = a1 + a2
  }

  val syntax = TexerSyntax(instance)

}

trait Texer[A] {
  self =>

  val blank = BlankToken
  val newln = NewLineToken

  def append(ts: Text[A], tht: Text[A]): Text[A] = tht ++ ts //this should shave any empty things from the right

  def append(tk: Token[A], tht: Text[A]): Text[A] = tht :+ tk

  def prepend(ts: Text[A], tht: Text[A]): Text[A] = ts ++ tht //this should shave any empty things from the left

  def prepend(tk: Token[A], tht: Text[A]): Text[A] = tk :: tht

  def lift(item: A): Token[A] = WordToken(item)

  def shaveLeft(text: Text[A])(amount: Int): Text[A] = text drop amount

  def shaveRight(text: Text[A])(amount: Int): Text[A] = text dropRight amount

  def manyOf(t: Token[A], i: Int): Text[A] = Stream.continually(t) take i toList

  //a law: tex.shaveL(tex.indent(_)(n))(_.isBlank) == _
  def shaveL(t: Text[A])(p: Token[A] => Boolean): Text[A] = t dropWhile p

  def shaveR(t: Text[A])(p: Token[A] => Boolean): Text[A] = t.reverse dropWhile p reverse //this should be implemented better

  def intersperse(t: Text[A])(tk: Token[A], interval: Int): Text[A] = partitioned(t)(interval).flatMap(l => append(tk, l))

  def intersperse2(t: Text[A])(tks: Text[A], interval: Int): Text[A] = partitioned(t)(interval).flatMap(t => append(tks, t))

  def colift(t: Token[A]): A = t match {
    case WordToken(a) => a
    case NewLineToken => newline
    case BlankToken => empty
  }

  def partitioned(t: Text[A])(at: Int): List[Text[A]] = t grouped at toList

  def indent(t: Text[A])(i: Int): Text[A] = prepend(manyOf(blank, i), t)

  def shaveBoth(t: Text[A])(p: Token[A] => Boolean): Text[A] = shaveR(shaveL(t)(p))(p)

  def length(t: Text[A]): Int = t.foldLeft(0)((a, b) => a + tokenSize(b))

  def relIndent(ts: Text[A], tht: Text[A])(pos: Int): Text[A] = append(indent(tht)(pos - length(ts)), ts)

  def spacing(t: Text[A])(i: Int): Text[A] = shaveBoth(intersperse2(t)(manyOf(blank, i), 1))(_.isBlank)

  def line(t: Text[A]): Text[A] = append(newln, t)

  def hspace(t: Text[A])(amount: Int): Text[A] = append(manyOf(newln, amount), t)

  def liftT(t: A): Text[A] = List(lift(t))

  //new lines should shave their left padding
  def lines(t: Text[A])(every: Int): Text[A] = {
    partitioned(t)(every)
      .map(shaveL(_)(_.isBlank))
      .flatMap(line)
  }

  def make(t: Text[A]): A = {
    shaveR(t)(_.isNewLine)
      .map(colift)
      .fold(zero)((a1, a2) => plus(a1, a2))
  }

  def tokenSize(a: Token[A]): Int = size(colift(a))

  def plus(a1: A, a2: A): A

  def size(t: A): Int

  def zero: A

  def empty: A

  def newline: A
}

case class TexerSyntax[A](tex: Texer[A]) {
  implicit def ops(t: Text[A]): TexerOps[A] = new TexerOps[A](t)(tex)
}

final class TexerOps[A](t: Text[A])(implicit tex: Texer[A]) {
  def append(ts: Text[A]): Text[A] = tex.append(ts, t)

  def prepend(ts: Text[A]): Text[A] = tex.prepend(ts, t)

  def lines(every: Int): Text[A] = tex.lines(t)(every)

  def line: Text[A] = tex.line(t)

  def shaveR(f: Token[A] => Boolean): Text[A] = tex.shaveR(t)(f)

  def spacing(by: Int): Text[A] = tex.spacing(t)(by)

  def indent(by: Int): Text[A] = tex.indent(t)(by)

  def hspace(by: Int): Text[A] = tex.hspace(t)(by)

  def relIndent(t2: Text[A])(pos: Int) = tex.relIndent(t, t2)(pos)

  def make: A = tex.make(t)
}


