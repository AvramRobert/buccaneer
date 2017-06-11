buccaneer
=
A (sort-of) library for writing command line applications quickly and effortlessly. 


#### For the impatient
The general idea in buccaneer is that you define commands very similarly to
how functions are defined in typed programming languages.

Let's start off by writing a simple command that adds two numbers together:

```scala
import buccaneer.core.DSL._
import buccaneer.core.Implicits._

val add = command("add")
val int = argument[Int]

val adder = (add - int - int)(_ + _)
```
As you can see, you define the name of your command, what parameters it takes and then associate a 
function block with that description. buccaneer makes sure that the associated function will always require 
the exact parameters and arity that you defined in your description.  
In the above example, the command takes 2 ints and therefore the associated function has an arity of two 
with types `(Int, Int)`. 

To run the command and print its result, you need only feed it 
to the command interpreter provided by the library and give it the list of arguments necessary in order
to call it:

```scala
import buccaneer.core.Interpreter

val input = List("add", "1", "2")

Interpreter.
  interpret(command).
  run(input).
  print
```
Any boilerplate associated with parsing arguments, interpreting commands and propagating errors is done
by the library. You need only concern yourself with the actual implementation, and not what happens in the 
background.


All the primitives required to build commands are:
* `command(<name>)` 
    * a command or subcommand identifier
* `argument[A]` 
    * an argument of some type `A`
* `option(<name>*)` 
    * an option identifier
* `assignment[A](<name>*)` 
    * an association between a name and a type (for things like `a=5`)

Compound things like Lists and Maps are just types, that you define as type arguments. For example: 
```scala
val listInts = argument[List[Int]]
val addList = (add - listInts)(_.sum)
```
<br />
So how does one build a complete command line interface?
<br />
Well, you aggregate all the commands you've defined in the following fashion:

```scala
import buccaneer.core.DSL._
import buccaneer.core.Implicits._
import buccaneer.core.Cli
import buccaneer.core.Interpreter

val add = command("add")
val subtract = command("subtract")
val int = argument[Int]
val double = argument[Double]
val r = option("-r", "--r")
val listInts = argument[List[Int]]

val interface: Cli[Int] = 
Cli(
(add - int - int)(_ + _),
(add - double - double)(_ + _),
(add - listInts)(_.sum),
(add - r - listInts)(_.sum),
(subtract - int - int)(_ - _),
(subtract - double - double)(_ - _))
```
This `Cli[Int]`-thing can also be interpreted and run. 
The interpreter is able to automatically pick the appropriate command and
run it, if the input matches any command from the interface:

```scala
def runPrint(input: List[String]) = {
  Interpreter.
  interpret(interface).
  run(input).
  print
}

runPrint(List("subtract", "2", "1"))
runPrint(List("add", "1", "2"))
runPrint(List("subtract", "2.012321", "1.2323"))
```
Finally, there is one additional interpreter called `interpretH`, which provides automatic MAN page generation and
input suggestions. These can be triggered at ANY point throughout the invocation. To trigger them, an input 
must always either end with `-help | --help` (for MAN pages) or `-sgst  | --sgst` (for suggestions). 
(*Note*: These keywords are configurable): 
```scala
def runPrintH(input: List[String]) = {
  Interpreter.
  interpretH(interface).
  run(input).
  print
}

runPrintH(List("subtract", "--help"))
runPrintH(List("subtract", "1", "--help"))
runPrintH(List("subtract", "--sgst"))
runPrintH(List("subtract", "1", "--sgst"))
```

What do they print? 

In general, `--help` would print something along the lines of: <br />
```bash
NAME
    <current command name> - <current command description>

USAGE
    <current command>  [<option1>] [<option2>] [<option3a> | <option3b>]
                       [<option4>] [<option5>] [...]

OPTIONS
    <option1>                         <option1 description>
    <option2>                         <option2 description>
    <option3a> | <option3b>           <option3 description>
    <option4>                         <option4 description>
    <option5>                         <option5 description>
    <...>                             <...>
    
SUB-COMMANDS
    <sub-command1>                    <sub-command1 description>
    <sub-command2>                    <sub-command2 description>
    <...>                             <...>
```

For our `subtract` command from earlier, `--sgst` would print something like: <br />
```bash
<int> <int> 
<Double> <Double>
```
And for a more specific input like `subtract 1 --sgst`, it would print:
```bash
1 <int>
```
<br />
And that's about it.
You can now create arbitrarily large and complex command line interfaces. 
Once you've defined one, you can feed it lists of arguments and run commands how and
when you want. 
<br />

To bind this
to an application for example, just pass the `args` your application receives to the interpreter:
```scala
def main(args: Array[String]): Unit = {
  ...
  runPrintH(args.toList)
}
```
To read about the full capabilities of this library, please take a look at the [docs](docs/documentation.md). <br />
Some more concrete and extended examples can be found in the [examples](src/main/scala/examples) package. 
