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
The general idea is that you simply express how your command should look like
and then associate a function with that description. One important thing to note is that the arity of the associated function
automatically grows with the number of type arguments your description requires. In the above example,
the command requires 2 ints and therefore the function arity is 2 with types `(Int, Int)`. 
<br />

To run the command, you need only feed it 
to the command interpreter provided by the library:

```scala
import buccaneer.core.Interpreter

val input = List("add", "1", "2")

Interpreter.
interpret(command).
run(input).
fold(println)(println)(println)
```
The reason for the `fold` is because the result of running a command can be one of three things.
Either:
* <b>Success</b>: you receive the result of the command function
* <b>Failure</b>: you receive a list of all the errors
* <b>Meta</b>: you receive a string containing some meta-information. This case in particular
is used for things like MAN pages and input suggestions (more on that later). 

Any boilerplate associated with parsing, interpreting commands and propagating errors is done
by the library. You need only concern yourself with actually implementing your commands.
<br />
All the primitives required to build commands are:
* `command(<name>)` - represents a command or subcommand identifier
* `argument[A]` - represents an argument of some type
* `option(<name>)` - represents an option identifier
* `assignment[A](<name>)` - represents an association between a name and a type (for things like `a=5`)

Compound things like Lists and Maps are just types, that you define as type arguments. For example: 
```scala
val listInts = argument[List[Int]]
val addList = (add - listInts)(_.sum)
```
<br />
So how does one build a complete command line interface?
<br />
Well, you define all commands like previously and then store them all together, like so:

```scala
import buccaneer.core.DSL._
import buccaneer.core.Implicits._
import buccaneer.core.Cli
import buccaneer.core.Interpreter

val add = command("add")
val subtract = command("subtract")
val int = argument[Int]
val double = argument[Double]
val r = option("-r" | "--r")
val listInts = argument[List[Int]]

val interface = 
Cli(
(add - int - int)(_ + _),
(add - double - double)(_ + _),
(add - listInts)(_.sum),
(add - r - listInts)(_.sum),
(subtract - int - int)(_ - _),
(subtract - double - double)(_ - _))
```
This `Cli[A]`-thing can also be interpreted and run. 
The interpreter is able to automatically pick the appropriate command and
run it, if the input matches any command from the interface:

```scala
def runPrint(input: List[String]) = {
  Interpreter.
  interpret(interface).
  run(input).
  fold(println)(println)(println)
}

runPrint(List("subtract", "2", "1"))
runPrint(List("add", "1", "2"))
runPrint(List("subtract", "2.012321", "1.2323"))
```
Finally, there is one additional interpretation function (`interpretH`), which provides dynamic MAN page generation and
input suggestions. They can be triggered at ANY point throughout the invocation. To trigger them, an input 
must always either end with `-help | --help` (for MAN pages) or `-sgst  | --sgst` (for suggestions):
```scala
def runPrintH(input: List[String]) = {
  Interpreter.
  interpretH(interface).
  run(input).
  fold(println)(println)(println)
}

runPrintH(List("subtract", "--help"))
runPrintH(List("subtract", "1", "--help"))
runPrintH(List("subtract", "--sgst"))
runPrintH(List("subtract", "1", "--sgst"))
```
And that's about it.
You can now create arbitrarily large and complex command line interfaces. To bind this
to any application, just pass the `args` your application receives to the interpreter.
A simple example would be:
```scala
def main(args: Array[String]): Unit = {
  ...
  runPrintH(args.toList)
}
```
Some more concrete and extended examples can be found in the `examples` package.
To read about the full capabilities of this library, please take a look at `<docs>`.
