### Introduction
buccaneer is a (rather opinionated) library for writing command line applications in a
very declarative way. It takes care of all the boilerplate associated with parsing commands,
error handling, command execution etc. and lets you only concern yourself with actually writing your 
command line interface. In addition, it also comes with some interesting features, that you get
for free.

### Commands
The basic idea in buccaneer is that commands are defined very similarly to how
functions are defined in most programming languages. They are _associations_ between 
a signature and a function block. The signature defines the function's name and 
domain (i.e. its _shape_), whilst the function block defines its behaviour and co-domain 
(i.e. what it should do and return when it is invoked).
<br />
<br />
This is essentially the main concept that pervades this entire library. You define a command
by expressing what is expected to be input, and then associate a function with that expectation.
That function will then naturally be run should the actual input match the expectation.
<br />
<br />
An example:
<br />
Let's assume that you want to write a naive command line application that is to
simulate a calculator. Naturally, the first thing you might want to support is the four
basic operations: addition, subtraction, multiplication and division. So let's encode them:
```scala
import buccaneer.core.DSL._
import buccaneer.core.Implicits._

val add = command("add")
val subtract = command("subtract")
val multiply = command("multiply")
val divide = command("divide")
val int = argument[Int]

val adder = (add - int - int)(_ + _) // add 1 2
val subtractor = (subtract - int - int)(_ - _) // subtract 2 1
val multiplier = (multiply - int - int)(_ * _) // multiply 2 3
val divider = (divide - int - int)(_ / _) // divide 2 2
```
As you can see, each command essentially _describes_ what it expects and has a
function block associated with each expectation. This is all typesafe and concrete. 
The function arity and parametricity to associate with each command signature is
automatically defined by the elements in that signature. More concretely: 
```scala

(add) { () => ... } //arity 0
(add - int) { a => ... } //arity 1, a: Int
(add - int - int) { (a, b) => ... } //arity 2, a,b: Int
(add - int - int - argument[Double]) { (a, b, c) => 
// arity 3, a,b: Int, c: Double
}
// and so on
```
Great. Now what about command options, or key-value associations?
<br />
Commands are built using only 4 primitives:
* `command(<name>)` 
    * a command or subcommand identifier
* `argument[A]` 
    * an argument of some type `A`
* `option(<name>)` 
    * an option identifier
* `assignment[A](<name>)` 
    * an association between a name and a type (for things like `a=5`)

**Note**: `argument[A]` and `assignment[A](<label>)` bump the arity 
of the associated function block. <br />
So how'd you use them? Well:
```scala 
val r = option("-r" | "--r")
val double = argument[Double]
val aDouble = assignment[Double]("a=")
val bDouble = assignment[Double]("b=")

val addRecursive = (add - r - int - int)(_ + _) // add -r 1 2
val addAssignedDouble = (add - aDouble - aDouble)(_ + _) // add a=2.0 b=4.0
```
And that's it. You simply mix these primitives together to create
more complex commands, and then associate a function
with that command. 

### Types and Values
When declaring a type argument or type assignment,
a `Read[A]` instance for that particular type is implicitly required. The `Read[A]` instance 
defines how a string should be converted to that
particular type. The `bucanneer.core.Implicits` package already contains `Read[A]` instances 
for a number of types. Please note, that some of them have some minor syntactic requirements in
order to avoid ambiguity:
* **String**
* **Boolean**
    * "true" and "false"
* **Int**
    * any natural number
    * Example: _1_ , _2_ , _3_ , _42_ .. <br /> 
    **Note** _1_ is an `Int` but _1.0_ is **NOT**
* **Double** 
    * any number with explicit decimal points 
    * Example: _1.0_, _54.23_, _102.2_ ..
* **Float** 
    * any number ending with "f" 
    * Example: _1f_, _2.5f_, _2390.234f_ .. 
* **BigInt** 
    * any natural number
    * same rules as for `Int`
* **BigDecimal** 
    * any number ending in "d"
    * Example: _2d_, _155.1230923829d_, _1.0d_ ..
* **Arbitrary collections**
    * comma-separated values
    * Example: _2,3,4,5,6,7,8_ or _true,true,true,false,true_ ..
* **Maps** 
    * comma-separated assignments using "=" 
    * Example: _a=4,b=5,c=7_ or _robert=true,carrie=false,lisa=false_ ..
* **Files** 
    * string paths
    * Example: _/home/myfiles/Downloads_

You can however also define your own instances:
```scala
import bucanneer.core.Read._

case class Fraction(num: Int, denom: Int)

implicit val readFraction = read[Fraction] { (input: String) => 
  if(input.contains("/") && input.split("/").length == 2) {
    val split = input.split("/")
    success(Fraction(split(0).toInt, split(1).toInt))
  } else {
    failure(s"Input of $input is not a fraction")
  }
}
```
Moreover, the concrete signature of a `Read[A]` conversion is `String => Result[A]`, where 
`Result[A]` is a scalaz `ValidationNel[Throwable, A]`. This means that you can actually 
accumulate all the errors when converting from  `String => A`.
For example:

```scala
import bucanneer.core.Read._
import scalaz.syntax.validation._

case class Fraction(num: Int, denom: Int)

// unsafeCoerce is a wrapper around try { } catch { }
def intable(input: String): Result[Int] = unsafeCoerce(input)(_.toInt)
  
implicit val readFraction = read[Fraction] { (input: String) => 
  if (input.contains("/") && input.split("/").length == 2) {
    val split = input.split("/")
    (intable(split(0)) |@| intable(split(1))) {
      (num, denom) => Fraction(num, denom)
    }
  } else {
    failure("Input is not a fraction.")
  }
}
```
### Running commands
So how do you actually _run_ your commands?
<br />
Commands are run by _interpreting_ a command line input relative to a command signature. What happens
is that the input is taken and matched against the command signature. If the input
conforms to the signature, then the function associated with that signature is run.
Otherwise, the inconsistencies and errors are accumulated and returned as a failure. 
<br />
<br />
buccaneer comes with an `Interpreter` that does all of this and
more. Let's take the previously defined calculator commands and run some of them:
```scala
import buccaneer.core.Interpreter

val goodInput = List("add", "1", "2")
val badInput = List("multiply", "false", "false")

Interpreter.
  interpret(adder).
  run(goodInput).
  fold(println)(println)(println) // => 3
  
Interpreter.
  interpret(multiplier).
  run(badInput).
  fold(println)(println)(println) // => List[Throwable] => ...
```
Right.. I'll show you in a second how you define a complete command
line interface and run individual commands from it, but first I would like to address the `fold`.
<br />
<br />
The reason we `fold` is because the result of an interpretation can be one of three things: 
* Success: `A => B` - the "happy" case. Here, the supplied input matched the signature and the 
function associated with it had been run. Now you are required to do something 
 with the result of that function, that is of type `A`. 
* Failure: `List[Throwable] => B` - the "sad" case. Here, the supplied input has either not 
matched the signature, or some parts of the input have been proven malformed. Now you are required to
do something with the errors that have been accumulated. 
* Meta: `String => B` - the "informative" case. This case is specifically reserved for when the 
interpretation yields some information about the command or the interface itself. In essence,
it is used for things like MAN pages and
input suggestions. For example, in the case of a MAN page, the string value is the page itself.

In most situations, the thing you generally do with each of these is that you print them.
Because this is somewhat the most common behaviour, buccaneer already provides a printing function that 
prints the result of a command in a conveniently formatted way:
```scala
Intepreter.
  interpret(multiplier).
  run(badInput).
  print
```
Should you however want to perhaps modify the result before printing, 
then the `fold` allows you directly handle the result of an interpretation. 

### Creating a command line interface
Now that we have commands and know how to run them, it is time to define a complete interface. 
A command line interface in buccaneer is just an aggregation of all the individual commands
you've defined so far: 
```scala
import buccaneer.core.Cli

val interface = Cli(adder, subtractor, multiplier, divider)
```
This `Cli[A]` thing is actually just a `Map` from command signatures to the 
commands themselves. One interesting thing about it is that its type parameter `A` will automatically
be inferred to `Any` if the result type of two or more commands does not line up.
<br />
<br />
To run command inputs against the whole interface, we need only feed it again to the `Interpreter`.
The `Interpreter` can both handle single commands, but also complete interfaces. In the
latter case, it will automatically resolve and find the command signature that
matches the given input, and run its associated function. If no command is found, or the input is 
malformed, then it shall appropriately return a failure:

```scala
val interpreter = Interpreter.interpret(interface)

def run(input: List[String]) = {
  interpreter.
    run(input).
    print
}

val input1 = List("subtract", "2", "1")
val input2 = List("divide", "1", "1")
val input3 = List("add", "x", true)

run(input1) // => 1
run(input2) // => 1
run(input3) // => No command found
```
And that's it. You can now create arbitrarily large and complex command
line interfaces. Before you start, you may however want to read the next
section. There are some interesting things that you get for free. 

### Man pages and input suggestions
Due to the way command signatures are modeled, buccaneer is able to generate MAN pages 
and offer command input suggestions dynamically and for free! 
##### MAN pages
Each primitive you use for composing commands may additionally receive a `String` description,
that is displayed in the MAN page representation of that command.
```scala
val add = command("add").msg("The add command")
val subtract = command("subtract").msg("The subtract command")
val int = argument[Int].msg("An integer argument")

val interface = Cli(
  (add - int - int)(_ + _),
  (subtract - int - int)(_ - _))
```
In order to receive MAN page support, you need only use a version of the interpreter that 
builds this feature in:
```scala
def print(input: List[String]) = {
Interpreter.
  interpretH(interface).
  run(input).
  print
}
```
Now, any time a command input ends with "-help" or "--help", 
the interpreter will compile a MAN page using the command
signatures and then print it. 
<br />
<b>Note</b>: "-help" or "--help" can be called at ANY point during command input.
```scala
val help1 = List("add", "--help")
val help2 = List("add", "1", "--help")

print(help1)
println(help2)
```
Both of these will print a variation of the following: 


##### Configuration 
You are additionally given the possibility to somewhat configure the outputted MAN page. 
The text width, column spacing and indentation of a MAN page are taken by the interpreter
from a case class as configuration. If you so desire, you can define your own 
configuration and feed it to the interpreter:
```scala
val myConfig = manConfig(textWidth = 200, 
                         indentation = 10, 
                         columnSpacing = 10)

def print(input: List[String]) = {
Interpreter.
    interpretH(interface, myConfig).
    run(input).
    print
}
```

##### Input suggestions
Again, due to the way commands signatures are modeled, the system is able to support 
dynamic input suggestions. Any time a command input ends with "-sgst" or
"--sgst", the interpreter will use the command signatures to compile a list of suggestions that 
partially match that given input. 
<br />
<b>Note</b>: Similar to MAN pages, these can be called at ANY point during
command input:
```scala
val suggest1 = List("add", "--sgst")
val suggest2 = List("subtract", "1", "--sgst")

print(suggest1) // => add <value> <value>
print(suggest2) // => add 1 <value>
```

### Macros (sort-of)
The command interpreter is _very_ similar to how a programming language compiler is built.
It is a composition of a series of phases, that take the result of the previous phase, validate
and transform it in some way, and then pass it on. In buccaneer, each phase is actually
a pure function that is composed with other pure functions. More concretely, they are
Kleisli compositions. 
<br />
<br />
Because the interpreter itself is just a composition of small functions, you can freely
and easily extend it or write your own. In essence you are able to add additional checks, extend
command behaviour and perform "interpretation-time" decisions.
<br />
Let me show you based on an example: 
<br />
--TODO--