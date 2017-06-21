buccaneer
=
A (sort-of) library for writing command line applications quickly and effortlessly. <br />
More descriptive information can be found here:
 * [Docs](docs/documentation.md) - for a full description of its capabilities
 * [Examples](examples) - for concrete usage examples


##### SBT
```scala
resolvers += Resolver.bintrayRepo("robertavram","maven")

libraryDependencies += "com.polymorph" %% "buccaneer" % "0.1.1"
```

#### For the impatient
The general idea in *buccaneer* is that you define commands very similarly to
how functions are defined in typed programming languages. They are associations between 
a signature and a function block.

Let's start off by writing a simple command that adds two numbers together:

```scala
import buccaneer.everything._
import buccaneer.Implicits._

val add = command("add")
val int = argument[Int]

val adder = (add - int - int)(_ + _)
```
As you can see, you define the name of your command, what parameters it takes and then associate a 
function block with them. *buccaneer* makes sure that the associated function will always require 
the exact arguments and arity that you've defined in your description.  
In the above example, the command takes 2 ints and therefore the associated function has an arity of two 
with types `(Int, Int)`. 

To run the command and print its result, you need only feed it 
to one of the provided command interpreters and give it the list of arguments necessary in order
to call it:

```scala
val input = List("add", "1", "2")

Interpreter.
  interpret(command).
  run(input).
  print
```
Any boilerplate associated with parsing arguments, interpreting commands and propagating errors is done
by the interpreter. You need only concern yourself with the actual implementation, and not what happens in the 
background.

#### Building commands 
All the primitives required for building commands are:
* `command(<name>)` 
    * a command or sub-command identifier
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

Additionally, arguments and assignments may also be constrained by a conditional, that futher check the input value: 
 ```scala
 
val posInt = argument[Int]((i: Int) => i > 0)
val posA = assignment[Int]((i: Int) => i > 0)("a=")
```
**Note:** The type has to be specified here due to Scala's left-to-right flowing type inference, which cannot properly infer
the type of the function. 
<br />
<br />
No argument or option commands are defined as commands of a single `Unit` argument:
```scala
val nothing = argument[Unit]

(nothing) {_ => println("I run with no input") }
```

#### Command line interfaces
So how does one define a complete command line interface?

Well, the commands you've defined are simply aggregated in the following fashion:

```scala
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
In this case, the interpreter automatically picks the appropriate command 
from the interface and runs it, should the input match any command from the interface. Otherwise it prints an error.

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
#### MAN pages and input suggestions
Finally, there is one last interpreter called `interpretH`, which provides automatic MAN page generation and
input suggestions. These can be triggered at any point throughout the invocation. To trigger them, an input 
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
<Int> <Int> 
<Double> <Double>
```
For a more specific input, e.g. `subtract 1 --sgst`, this get's narrowed down appropriately:
```bash
<Int> <Int>
```
#### Final thoughts
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
For a bit more insight, I advise taking a look at either the [docs](docs/documentation.md) or [examples](examples). (or both)