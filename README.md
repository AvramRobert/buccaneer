buccaneer
=
A library for writing command line applications quickly and effortlessly. 


#### For the impatient
Let's start by writing a simple command that adds two numbers together.

```scala
import buccaneer.core.DSL._
import buccaneer.core.Implicits._

val add = command("add")
val int = argument[Int]

val command = (add - int - int)(_ + _)
```
And that's it. This command both describes what input is expected, but also what
needs to happen once that input has been provided. To run it, you need only feed it 
to the command interpreter provided by the library:

```scala
import buccaneer.core.Interpreter

val input = List("add", "1", "2")

Interpreter.interpret(command).run(input)
```
Bad inputs are automatically caught by the interpreter and a failure is propagated.
Any boilerplate associated with parsing, interpreting commands and reporting errors is done
by the library. You need only concern yourself with actually implementing your commands.
<br />
All the primitives required to build commands are:
* `command(<name>)` - represents a command or subcommand name
* `argument[A]` - represents an argument of some type
* `option(<name>)` - represents an option 
* `assignment[A](<name>)` - represents an association between a name and a type (for things like `a=5`)

Compound things like Lists and Maps are just types, that you require  in an argument or assignment. For example: 
```scala
val listInts = argument[List[Int]]
val addList = (add - listInts)(_.sum)
```
<br />
So how does one build a complete command line interface?
<br />
Well, you define all commands like previously and then store them like so:

```scala
import buccaneer.core.DSL._
import buccaneer.core.Implicits._
import buccaneer.core.Cli
import buccaneer.core.Interpreter

val add = command("add")
val subtract = command("subtract")
val int = argument[Int]
val double = argument[Double]
val listInts = argument[List[Int]]

val interface = 
Cli(
(add - int - int)(_ + _),
(add - double - double)(_ + _),
(add - listInts)(_.sum),
(subtract - int - int)(_ - _),
(subtract - double - double)(_ - _))
```
Similar to the previous example with one command, this `Cli[A]`-thing can also be interpreted
and run. The interpreter will automatically pick the appropriate command and
run it, if the input matches any command from the interface:

```scala
val interpreter = Interpreter.interpret(interface)

interpreter.run(List("subtract", "2", "1"))
interpreter.run(List("add", "1", "2"))
interpreter.run(List("subtract", "2.012321", "1.2323"))
```
Finally, there is one additional interpretation function (`interpretH`), which provides dynamic MAN page generation and
input suggestions (these features come for free). 
They can be triggered at ANY point throughout the invocation. To trigger them, an input 
must always either end with `-help | --help` (for MAN pages) or `-sgst  | --sgst` (for suggestions):
```scala
val interpreter = Interpreter.interpretH(interface)

interpreter.run(List("subtract", "--help"))
interpreter.run(List("subtract", "1", "--help"))
interpreter.run(List("subtract", "--sgst"))
interpreter.run(List("subtract", "1", "--sgst"))
```
And that's about it.
You can now create arbitrarily large and complex command line interfaces. To bind this
to any application, just pass the `args` your application receives to the interpreter.
A simple example would be:
```scala
def main(args: Array[String]): Unit = {
  ...
  interpreter.run(args.toList)
}
```
Some more concrete and extended examples can be found in the `examples` package.
To read about the full capabilities of this library, please take a look at `<docs>`.
