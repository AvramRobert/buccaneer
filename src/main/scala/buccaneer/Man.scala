package buccaneer

import Formatter.Formatter
import scalaz.syntax.traverse._
import scala.annotation.tailrec
import scalaz.Reader
import ManConfig._

trait ManOps {

  /** Convenience DSL function for creating creating ManConfig
    *
    * @param programName        command line application name
    * @param programDescription a description of the application
    * @param help               the identifier to call when printing the help MAN page
    * @param suggest            the identifier to call when printing the list of input suggestions
    * @param textWidth          desired text width per block of text
    * @param indentation        desired indentation per line
    * @param columnSpacing      desired spacing between columns of text
    * @return a `ManConfig` case class containing all the provided values
    */
  def manpage(programName: String = "nameless",
          programDescription: String = "descriptionless",
          help: Identifier = help,
          suggest: Identifier = suggest,
          textWidth: Int = 150,
          indentation: Int = 5,
          columnSpacing: Int = 5): ManConfig = man(programName, programDescription, help, suggest, textWidth, indentation, columnSpacing)
}


object ManConfig {
  lazy val helpSym = Label("-help") | Label("--help")
  lazy val helpDesc = "Prints this page"
  lazy val suggestSym = Label("-sgst") | Label("--sgst")
  lazy val suggestDesc = "Prints a list of input suggestions based on the current input"

  lazy val help = Denot.id(helpSym).msg(helpDesc)
  lazy val suggest = Denot.id(suggestSym).msg(suggestDesc)

  def man(programName: String = "nameless",
          programDescription: String = "descriptionless",
          help: Identifier = help,
          suggest: Identifier = suggest,
          textWidth: Int = 150,
          indentation: Int = 5,
          columnSpacing: Int = 5): ManConfig =
    ManConfig(programName, programDescription, help, suggest, textWidth, indentation, columnSpacing)
}

/** A record containing various parameters for configuring the aesthetic of a MAN page.
  *
  * @param programName        command line application name
  * @param programDescription a description of the application
  * @param help               the identifier to call when printing the help MAN page
  * @param suggest            the identifier to call when printing the list of input suggestions
  * @param textWidth          desired text width per block of text
  * @param indentation        desired indentation per line
  * @param columnSpacing      desired spacing between columns of text
  */
case class ManConfig(programName: String,
                     programDescription: String,
                     help: Identifier,
                     suggest: Identifier,
                     textWidth: Int,
                     indentation: Int,
                     columnSpacing: Int)

object Man {

  type Section[A] = Reader[ManConfig, A]

  lazy val missing = Vector.empty[Formatter]

  def section[A](f: ManConfig => A): Section[A] = Reader(f)

  /** Creates a empty section of text, that will return an empty formatter
    * when given a `ManConfig`.
    *
    * @return a section of text that can be formatted
    */
  def emptySection: Section[Formatter] = section(_ => Formatter.empty)

  /** Creates a formatter from a string.
    *
    * @param s piece of text to format
    * @return a formatter for `s`
    */
  def text(s: String): Formatter = Formatter(s.toCharArray.toVector)

  /** Creates an indented and padded block of text given a `ManConfig`
    *
    * @param txt   block of text
    * @param fsize size adjustment
    * @return a section of text that formats the block when given a `ManConfig`
    */
  def line(txt: String)(fsize: ManConfig => Int): Section[Formatter] = section { config =>
    text(txt).
      push(config.indentation).
      every.
      ofWidth(config.indentation + fsize(config)).
      fill
  }

  /** Creates two columns of text that are indented, padded and aligned.
    * The size of the largest text block is also passed in case that block
    * is not one of the two in the argument list. (maintains size consistency)
    *
    * @param left    text block on the left
    * @param right   text block on the right
    * @param largest size of the largest text block (if there are any
    * @return a section of text that formats the block when given a `ManConfig`
    */
  def columned(left: String, right: String, largest: Int): Section[Formatter] = for {
    key <- line(left)(config => largest + config.indentation)
    value <- line(right)(config => config.textWidth - config.columnSpacing - config.indentation - largest)
    aligned <- section { config => key.align(value, config.columnSpacing) }
  } yield aligned

  /** Pairs the elements of a command with their documentation text.
    *
    * @param command command shape
    * @return a vector of tuples containing the element string form and its documentation
    */
  def paired(command: Tree[Denot]): Vector[(String, String)] = {
    @tailrec def go(cur: Tree[Denot], acc: Vector[(String, String)] = Vector()): Vector[(String, String)] = cur match {
      case a -< (l, r) =>
        val left = l.foldLeft(a.show) { (str, denot) => s"$str ${denot.show}" }
        go(r, acc :+ (left, a.docs))
      case Leaf => acc
    }

    go(command)
  }

  /** Given a number of command shapes, returns the size of the command
    * whose string representation is largest.
    *
    * @param all vector of command shapes
    * @return size of the largest from `all`
    */
  def largest(all: Vector[Tree[Denot]]): Int = all.
    map {
      case a -< (l, _) => l.foldLeft(a.show.length)((x, y) => x + y.show.length) + 1
      case Leaf => 0
    }.max

  /** Creates the command section of a MAN page.
    *
    * @param tree command shape to extract infromation from
    * @return section of text that will create the formatted block of text when given a `ManConfig`
    */
  def command(tree: Tree[Denot]): Section[Formatter] = section { config =>
    tree.
      takeWhile(_.isMajorIdentifier).
      toVector.
      lastOption.
      map(denot => text(s"${denot.show} - ${denot.docs}")).
      getOrElse(text(s"${config.programName} - ${config.programDescription}")).
      ofWidth(config.textWidth).
      push(config.indentation).
      every
  }

  /** Creates the usage section of a MAN page.
    *
    * @param command current command
    * @param options the options of the current command
    * @return a section containing a compacted usage block in text form
    */
  def usages(command: Tree[Denot], options: Vector[Tree[Denot]]): Section[Vector[Formatter]] = section { config =>
    val internal = Vector(Tree(config.help), Tree(config.suggest))

    (options ++ internal).
      filterNot(_.rootOf(_.isMajorIdentifier)).
      flatMap(_.toVector).
      map(_.show).
      distinct.
      map(str => s"[$str]").
      grouped(3).
      map(v => text(v.mkString(" ")).widen(_ + 1)).
      reduceOption(_ interlace _).
      map { usages =>
        text(s"${config.programName} ${command.toVector.map(_.show).mkString(" ")}").
          push(config.indentation).
          widen(_ + config.indentation).
          align(usages, 1)
      }.
      map(Vector(_)).
      getOrElse(missing)
  }

  /** Creates the subcommand section of a MAN page.
    *
    * @param all command shapes
    * @return a section containing a vector of all subcommands in text form
    */
  def subcommands(all: Vector[Tree[Denot]]): Section[Vector[Formatter]] = {
    val max = largest(all)
    all.filter(_.rootOf(_.isMajorIdentifier)).
      map(_.rootOption).
      distinct.
      map(_.fold(emptySection) { denot =>
        columned(denot.show, denot.docs, max)
      }).
      sequenceU
  }

  /** Creates the option section of a MAN page.
    *
    * @param all command shapes
    * @return a section containing a vector of all options in text form
    */
  def options(all: Vector[Tree[Denot]]): Section[Vector[Formatter]] = section { config =>
    val max = largest(all)
    val internal = Vector(Tree(config.help), Tree(config.suggest))
    (all ++ internal).
      filterNot(_.rootOf(_.isMajorIdentifier)).
      flatMap(paired).
      distinct.
      map(t => columned(t._1, t._2, max)).
      sequenceU
  }.flatMap(identity)

  /** Creates a complete text from a collection of formatters.
    *
    * @param formatters formatters with formatted text
    * @return a string containing all formatted texts
    */
  def makeText(formatters: TraversableOnce[Formatter]): String = {
    formatters.foldLeft(Vector.empty[Char]) { (acc, frmt) => acc ++ frmt.runH }.mkString("")
  }

  /** Extracts the formatted text from a formatter.
    *
    * @param f formatter with formatted text
    * @return text of `f`
    */
  def makeText(f: Formatter): String = makeText(List(f))

  /** `getOrElse`-like function for empty collections of formatters.
    * Lifts `txt` into a formatter when `v` is empty and returns that.
    * Otherwise returns `v`.
    *
    * @param v   possibly empty vector of formatters
    * @param txt alternative if `v` is empty
    * @return vector of formatters
    */
  def whenEmpty(v: Vector[Formatter])(txt: => String): Vector[Formatter] = {
    if (v.isEmpty) Vector(text(txt))
    else v
  }

  /** Creates a complete MAN page relative to some concrete command
    * line input and the command shapes that match that input.
    *
    * @param input         input from command line
    * @param corresponding shapes of commands corresponding to input
    * @return section of text that returns the MAN page when given a `ManConfig`
    */
  def help(input: List[String], corresponding: Set[Tree[Denot]]): Section[String] = for {
    suggestion <- section(_.suggest)
    matched <- section(_ => corresponding.toVector.map(_ zips input))
    first = matched.map(_.takeWhile(_._2.isDefined).map(_._1))
    rest = matched.map(_.dropWhile(_._2.isDefined).map(_._1))
    commandSection <- first.headOption.fold(emptySection)(command)
    usagesSection <- first.headOption.fold(emptySection.map(_ => missing))(usages(_, rest))
    optionsSection <- options(rest)
    subcommandSection <- subcommands(rest)
    linebreak = Formatter.empty
  } yield makeText {
    (text("NAME") +:
      commandSection +:
      linebreak +:
      text("USAGE") +:
      whenEmpty(usagesSection)("There are no usages available.")) ++
      (linebreak +:
      text("OPTIONS") +:
      whenEmpty(optionsSection)("There are no options available.")) ++
      (linebreak +:
        text("SUB-COMMANDS") +:
        whenEmpty(subcommandSection)("There are no sub-commands available.") :+
        linebreak :+
        text(s"Hint: You can invoke ${suggestion.show} at any point to receive a list of all possible commands that match your current input."))
  }

  /** Creates suggestions relative to some concrete command line input
    * and the shape of commands corresponding to that input.
    *
    * @param input         input from command line
    * @param corresponding shape of commands corresponding to input
    * @return a section of text that returns the box of suggestions given a `ManConfig`
    */
  def suggest(input: List[String], corresponding: Set[Tree[Denot]]): Section[String] = section { config =>
    makeText {
      corresponding.
        map { tree =>
          text(tree.string(" ") {
            case id@Identifier(Alternative(alts), _, _) if alts.size > 1 => s"[${id.show}]"
            case tid@TypedIdentifier(Alternative(alts), _, _) if alts.size > 1 => s"[${tid.show}]"
            case denot => denot.show
          })
        }
    }
  }
}

object Formatter {

  private[buccaneer]
  sealed trait Cardinality

  private[buccaneer]
  case object All extends Cardinality

  private[buccaneer]
  case class Few(n: Int) extends Cardinality

  type Format[A] = Vector[A] => Vector[A]

  def apply(data: Vector[Char]): Formatter = More(data, identity, data.size, 0, Few(1))

  def apply(text: String): Formatter = apply(text.toCharArray.toVector)

  def empty: Formatter = Formatter(Vector(' '))

  def emptyN(n: Int): Formatter = {
    if (n <= 0) empty
    else empty.append(' ').repeat(n - 1)
  }

  /** An ADT for describing text formatting in a combinator-like fashion.
    * Formatting is defined in terms of what needs to happen at each line.
    */
  private[buccaneer]
  sealed trait Formatter {
    protected def data: Vector[Char]

    def width: Int

    val blank: Char = ' '

    val break: Char = '\n'

    val hyphen: Char = '-'

    /** Size of the text to format.
      *
      * @return size of text
      */
    def breadth: Int = data.size

    /** Calculates the total amount of lines to format given the predefined text width.
      *
      * @return amount of lines
      */
    def totalLines: Int = Math.ceil(breadth.toDouble / width.toDouble).toInt

    /** Folds over the possible types of formatter and applies an anamorphism.
      *
      * @param f transformation of an every-line formatter
      * @param g transformation of a many-line formatter
      * @tparam B type of resulting transformation
      * @return new value
      */
    def fold[B](f: Every => B)(g: More => B): B = this match {
      case every@Every(_, _, _) => f(every)
      case more@More(_, _, _, _, _) => g(more)
    }

    /** Transforms one formatting function into another
      *
      * @param f morphism for the formatting
      * @return a new formatter containing the new formattign function
      */
    def endo(f: Format[Char] => Format[Char]): Formatter = fold[Formatter](e => e.copy(f = f(e.f)))(m => m.copy(f = f(m.f)))

    /** Extends the current formatting by the given function.
      * Composes `f` with the current formatting.
      *
      * @param f formatting function
      * @return a new formatter with `f` composed into it
      */
    def continue(f: Format[Char]): Formatter = endo(_ andThen f)

    /** Prepends a character to the formatted text.
      *
      * @param c character to prepend
      * @return a new formatter with the character prepended to its text
      */
    def prepend(c: Char): Formatter = continue(v => c +: v)

    /** Appends a character to the formatted text.
      *
      * @param c character to append
      * @return a new formatter with the character appended to its text
      */
    def append(c: Char): Formatter = continue(_ :+ c)

    /** Applies the formatting to every line of its text.
      *
      * @return a new formatter that applies its formatting to every line of text
      */
    def every: Formatter = fold(identity)(x => Every(x.data, x.f, x.width))

    /** Applies the formatting to just the first line of its text.
      *
      * @return a new formatter that applies its formatting to the first line of tex
      */
    def one: Formatter = fold(e => More(e.data, e.f, e.width, 0, Few(1)))(m => More(m.data, m.f, m.width, m.at, Few(1)))

    /** Repeats a formatting for the given amount of times.
      *
      * @param amount amount of repetitions
      * @return a new formatter that applies its formatting `amount` of times
      */
    def repeat(amount: Int): Formatter = {
      @tailrec def go(v: Vector[Char], f: Format[Char], n: Int): Vector[Char] = {
        if (n <= 0) v
        else go(f(v), f, n - 1)
      }

      endo(f => (v: Vector[Char]) => go(v, f, amount))
    }

    /** Repeats a formatting as long as the predicate asserts true.
      *
      * @param p predicate stating the condition of repetition
      * @return a new formatter that applies its formatting for as long as `p` asserts true
      */
    def repeatWhile(p: Vector[Char] => Boolean): Formatter = {
      @tailrec def go(v: Vector[Char], f: Format[Char]): Vector[Char] = {
        if (p(v)) go(f(v), f)
        else v
      }

      endo(f => (v: Vector[Char]) => go(v, f))
    }

    /** Enhances the line width in relation to the current one.
      *
      * @param f enhancement function
      * @return a new formatter for whose line width is given by `f`
      */
    def widen(f: Int => Int): Formatter = fold[Formatter](e => e.copy(width = f(e.width)))(m => m.copy(width = f(m.width)))

    /** Appends a character to the formatted text for as long as the predicate asserts true.
      *
      * @param p predicate stating the condition of repetition
      * @param c character to append
      * @return a new formatter, that appends `c` as long as `p` asserts true
      */
    def appendWhile(p: Vector[Char] => Boolean)(c: Char): Formatter = coeval.append(c).repeatWhile(p)

    /** Pads the end of the formatted text with `n` given amount of blank characters.
      *
      * @param n amount of blank characters
      * @return a new formatter, that fills the end of its text with `n` blank characters
      */
    def fill(n: Int): Formatter = concat(emptyN(n))

    //This is essentially the place where monads would come in handy
    //In this scenario, if i do not pre-evaluate the thing, it will repeat the total formatting function, as
    // it is not limited to only the last operation. So if i do something like formatter.prepend('c').fill(1).repeat(3) ;
    // it is going to both prepend 'c' and fill once 3 times on all lines.
    // The point of a monad in this case would be to isolate the context and allow for things to compose
    // without them affecting each other.
    /** Pads the formatted text with empty characters until its predefined line width is achieved.
      *
      * @return a new formatter, that fills its text lines with blanks until its predefined line width is achieved
      */
    def fill: Formatter = appendWhile(_.size < width)(blank).every

    /** Prepends `n` blanks to the first line of the formatted text.
      *
      * @param n amount of blank characters
      * @return a new formatter, that prepends `n` blank characters to the first line of formatted text
      */
    def push(n: Int): Formatter = prepend(blank).repeat(n)

    /** Evaluates a given formatting based on its specification.
      * The resulting text is not divided into lines.
      *
      * @return the formatted text as a vector of characters
      */
    def evaluate: Vector[Char] = this match {
      case Every(data, f, width) => More(data, f, width, 0, All).evaluate
      case More(data, f, width, at, All) if at < data.size => More(data, f, width, 0, Few(totalLines)).evaluate
      case More(data, f, 0, at, _) => f(data)
      case More(data, f, width, at, Few(i)) if at < data.size && i > 0 =>
        val cursor = (data grouped width).toVector
        (cursor.take(at) ++ cursor.slice(at, at + i).map(f) ++ cursor.drop(at + i)).flatten
      case More(data, _, _, _, _) => data
    }

    /** Evaluates a given formatting but also includes hyphenation of text.
      * The resulting text is not divided into lines.
      *
      * @return the formatted, hyphenated text as a vector of characters
      */
    def evaluateH: Vector[Char] = hyphenate.evaluate

    /** Concatenates to formatted texts.
      *
      * @param that formatter containing the text to concatenate
      * @return a new formatter containing the concatenation of `this` and `that` formatted text
      */
    def concat(that: Formatter): Formatter = coeval.fold[Formatter] {
      every => every.copy(data = every.data ++ that.evaluate)
    } {
      more => more.copy(data = more.data ++ that.evaluate)
    }

    // This should describe what needs to happen, not actually do it
    /** Interleaves the lines of two formatted texts.
      *
      * @param that formatter containing the lines to interleave with
      * @return a new formatter containing the interleaved lines of `this` and `that` formatter
      */
    def interleave(that: Formatter): Formatter = {
      Formatter(
        evaluate.
          grouped(width).
          zip(that.evaluate.grouped(that.width))
          .flatMap(v => v._1 ++ v._2)
          .toVector)
        .ofWidth(width + that.width)
    }

    /** Attaches `that` to `this` with a new line and makes
      * sure that the overall width is kept.
      *
      * @param that formatter to interlace
      * @return `that` formatter placed at the bottom of `this` formatter
      */

    def interlace(that: Formatter): Formatter = {
      val larger = Math.max(width, that.width)

      ofWidth(larger).
        fill.
        concat(that.ofWidth(larger).fill).
        ofWidth(larger)
    }

    //TODO: Vector() should be the identity in this thing! fill(0) for example actually fills the bloody thing with one value (' ')
    /** Equalises the number of lines between two formatters.
      * Essentially it pads smaller formatters until they equal
      * (in amount of lines) their larger counterparts.
      *
      * @param that formatter to equate with
      * @return two formatters equal in number of lines
      */
    def equate(that: Formatter): (Formatter, Formatter) = {
      def pad(larger: Formatter, smaller: Formatter) = {
        val diff = larger.totalLines - smaller.totalLines
        if (diff == 0) (larger.fill, smaller.fill)
        else (larger.fill, smaller.fill.fill(diff * smaller.width))
      }

      if (totalLines >= that.totalLines) pad(this, that)
      else pad(that, this).swap
    }

    /** Aligns two formatters such that their texts are
      * properly aligned next to each other. It essentially
      * places the texts of these formatters in columns and
      * aligns them next to each other relative to the given `distance`
      *
      * @param that     formatter to align with
      * @param distance distance between both columns
      * @return a new formatter containing and formatting two columned texts
      */
    def align(that: Formatter, distance: Int): Formatter = {
      val (h1, h2) = coevalH equate that.coevalH

      val empty = emptyN(h1.totalLines * distance).ofWidth(distance)
      h1 interleave empty interleave h2
    }

    /** Splits the text into lines of predefined width
      *
      * @return a new formatter with its text splitted into lines
      */
    def withLines: Formatter = append(break)

    /** Resets the line width of the formatter.
      *
      * @param i desired new line width
      * @return a new formatter containing the new line width
      */
    def ofWidth(i: Int): Formatter = widen(_ => i)

    /** Runs the fromatting a returns a complete text
      * as a String. Does not hyphenate
      *
      * @return the formatted text as a string
      */
    def runMake: String = run.mkString("")

    /** Runs the formatting and returns the complete text,
      * divided into lines, as a vector of characters. Does not hyphenate.
      *
      * @return the formatted text as a vector of characters
      */
    def run: Vector[Char] = withLines.every.evaluate

    /** Runs the formatting and returns the text,
      * divided into lines, as a vector of characters. Also hyphenates the text.
      *
      * @return the formatted and hyphenated text as a vector of characters
      */
    def runH: Vector[Char] = coevalH.run

    /** Hyphenates the formatted text.
      *
      * @return a new formatter that hyphenates its text
      */
    def hyphenate: Formatter = {
      @tailrec def go(rem: Vector[Char], h: Vector[Char] = Vector()): Vector[Char] = {
        if (rem.isEmpty) h
        else (h, rem) match {
          case (_ :+ `blank`, `blank` +: _) => go(rem.drop(width), h ++ rem.take(width))
          case (_, `blank` +: _) => go(rem.drop(width), h ++ rem.take(width))
          case (_ :+ `blank`, _) => go(rem.drop(width), h ++ rem.take(width))
          case (_ :+ `hyphen`, _) => go(rem.drop(width), h ++ rem.take(width))
          case (_, `hyphen` +: _) => go(rem.drop(width), h ++ rem.take(width))
          case (a :+ `blank` :+ c, b) => go(c +: b, a :+ blank :+ blank)
          case (a :+ c1, c2 +: b) => go(c1 +: c2 +: b, a :+ hyphen)
          case _ => go(rem.drop(width), h ++ rem.take(width))
        }
      }

      Formatter(go(evaluate)).ofWidth(width)
    }

    /** Evaluates the formatter and lifts the resulting text into
      * a single-line formatter.
      *
      * @return a formatter containing the previously evaluated formatting
      */
    def coeval: Formatter = Formatter(evaluate).ofWidth(width)

    /** Evaluates the formatter with hyphenation and lifts the resulting
      * text into a single-line formatter.
      *
      * @return a formatter containing the previously evaluated and hyphenated formatting
      */
    def coevalH: Formatter = Formatter(evaluateH).ofWidth(width)
  }

  /** Indicates a formatting that needs to happen at every line in some text.
    *
    * @param data  text to format
    * @param f     the formatting that should occur at every line
    * @param width the width of each line for the given `data`
    */
  private[buccaneer]
  case class Every(data: Vector[Char], f: Format[Char], width: Int) extends Formatter

  /** Indicates a formatting that needs to happen at specific lines in some text.
    *
    * @param data  text to format
    * @param f     the formatting that should occur only at specific lines
    * @param width the width f each line for the given `data`
    * @param at    line at which formatting should start
    * @param n     amount of lines for which formatting should be applied
    */
  private[buccaneer]
  case class More(data: Vector[Char], f: Format[Char], width: Int, at: Int, n: Cardinality) extends Formatter

}