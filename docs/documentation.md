### Introduction
*buccaneer* is a (rather opinionated) pseudo-library for writing command line applications in a
very declarative way. Its main goal is to take care of all the boilerplate associated with argument parsing,
error handling, command execution etc. and let the user only concern himself with actually writing the
command line interface. In addition, it provides some other interesting features, that come for free.

### Commands
The basic idea in *buccaneer* is that commands are defined very similarly to how
functions are defined in typed programming languages. They are _associations_ between 
a signature and a function block. The signature defines the function's name and 
domain, whilst the function block (and return) defines its co-domain.
<br />
<br />
This is essentially the main concept that pervades this entire library. You define a command
by expressing what is expected to be input, and then associate a function with that expectation.
That function will then naturally be run should the actual input match the expectation.

##### Example

Let's assume that you want to write a naive command line application that is to
simulate a calculator. Naturally, the first thing you might want to support is the four
basic operations: addition, subtraction, multiplication and division. So let's encode them:
```scala
import buccaneer.everything._
import buccaneer.Implicits._

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
function block associated with each expectation. *buccaneer* also makes sure that the function
block you associate also has the arity and parametricity you specified in the description. <br />
More concretely: 
```scala

(add) { () => ... } //arity 0
(add - int) { a => ... } //arity 1, a: Int
(add - int - int) { (a, b) => ... } //arity 2, a,b: Int
(add - int - int - argument[Double]) { (a, b, c) => 
// arity 3, a,b: Int, c: Double
}
// and so on
```
#### Building commands
Commands are built using only 4 primitives:
* `command(<name>)` 
    * a command or sub-command identifier
* `argument[A]` 
    * an argument of some type `A`
* `option(<name>*)` 
    * an option identifier
    * it accepts a variable number of names and treats them as alternatives
* `assignment[A](<name>)` 
    * an association between a name and a type (for things like `a=5`)
    * it accepts a variable number of names and treats them as alternatives
    
Additionally, arguments and assignments may accept conditionals, that further check the input value:
```scala
val posInt = argument[Int]((i: Int) => i > 0)
val posAssign = assignment[Int]((i: Int) => i > 0)("a=")
```
The reason you  have to specify a type here is because of Scala's left-to-right
flowing type inference, which cannot infer a lambda's type if it
is defined within the first parameter list. 

**Usage**
<br />
So now, how'd you use them? Well:
```scala 
val r = option("-r", "--r")
val double = argument[Double]
val aDouble = assignment[Double]("a=")
val bDouble = assignment[Double]("b=")

val addRecursive = (add - r - int - int)(_ + _) // add -r 1 2
val addAssignedDouble = (add - aDouble - aDouble)(_ + _) // add a=2.0 b=4.0
```


**No argument commands**
<br />
What if I want a command that takes no arguments or options? 
In this case, you would be running your program effectively with no input. <br />
In general, functions with no arguments are essentially functions from `Unit => A`. <br />
*buccanneer* has the same behaviour:
```scala
val nothing = argument[Unit]

val withoutInput = (nothing) { () => 
  println("I hope you've not expected much.") 
}    
```

And that's it. All you have to do is simply mix these primitives together to create
more complex commands. 

### Types and Values
When declaring a type argument or type assignment,
a `Read[A]` instance for that particular type is implicitly required. The `Read[A]` instance 
defines how a string should be converted to that
particular type. Additionally it also specifies a string representation for `A`. 
The `buccaneer.Implicits` package already contains `Read[A]` instances 
for a number of types. Please note, that some of them have some minor syntactic requirements in
order to avoid ambiguity. <br />
* **Unit**
* **String**
* **Boolean**
    * "true" and "false"
* **Int**
    * any natural number
    * Example: _1_ , _2_ , _3_ , _42_ ..
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
    * empty collections are ignored
    * Example: _2,3,4,5,6,7,8_ or _true,true,true,false,true_ ..
* **Maps** 
    * comma-separated assignments using "=" 
    * empty maps are ignored
    * Example: _a=4,b=5,c=7_ or _robert=true,carrie=false,lisa=false_ ..
* **Files** 
    * string paths
    * Example: _/home/myfiles/Downloads_


You can however also define your own instances. <br />
**Note**: Because `Read[A]` instances also contain a string representation
of their type `A`, this has to also be specified with its definition.

```scala
import buccaneer.Read._

case class Fraction(num: Int, denom: Int)

implicit val readFraction: Read[Fraction] = read("Fraction") { (input: String) => 
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
import buccaneer.Read._
import buccaneer.Implicits.readInt
import scalaz.syntax.validation._

case class Fraction(num: Int, denom: Int)

implicit val readFraction: Read[Fraction] = read("Fraction") { (input: String) => 
  if (input.contains("/") && input.split("/").length == 2) {
    val split = input.split("/")
    (readInt(split(0)) |@| readInt(split(1))) {
      (num, denom) => Fraction(num, denom)
    }
  } else {
    failure("Input is not a fraction.")
  }
}
```

### Running commands
Commands are run by _interpreting_ a command line input relative to a command signature. 
The input is taken and matched against that signature. If the input
conforms to the signature, then the function associated with it is run.
Otherwise, the inconsistencies and errors are accumulated and returned as a failure. 
<br />
<br />
*buccaneer* comes with a number of interpreters that do all of this and
more. Let's take the previously defined calculator commands and run some of them:
```scala
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
Wait, why the fold? <br />
The reason we `fold` is because the result of an interpretation can be one of three things: 
* Success: `A => B` - the "happy" case. Here, the supplied input matched the signature and the 
function associated with it had been run. Now you are required to do something 
 with the result of that function, that is of type `A`. 
* Failure: `List[Throwable] => B` - the "sad" case. Here, the supplied input has either not 
matched the signature, or some parts of the input have been proven malformed. Now you are required to
do something with the errors that have been accumulated. 
* Meta: `String => B` - the "informative" case. This case is specifically reserved for when the 
interpretation yields some information about the command or the interface itself. In essence,
it is used for things like MAN pages. For example, in the case of a MAN page, the string value is the page itself.

In most situations, the thing you generally do with each of these is that you print them.
Because this is somewhat the most common behaviour, *buccaneer* already provides a printing function that 
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
A command line interface in *buccaneer* is just an aggregation of all the individual commands
you've defined so far: 
```scala
val interface: Cli[Int] = Cli(adder, subtractor, multiplier, divider)
```
This `Cli[Int]`-thing is actually just a `Map` from command signatures to the 
commands themselves. One interesting thing about it is that its type parameter will automatically
be inferred to `Any` if the result type of two or more commands are not homogeneous.
<br />
<br />
To run command inputs against the whole interface, we need only feed it to one of the interpreters.
There are interpreters for handling both single commands, but also complete interfaces. In the
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
**Note:** Because the intepreter has to essentially first determine which command signature best fits the
provided input, it does not know a priori what exact description will match. As such, in the case of
complete interfaces, it cannot aggregate errors and output them. If it were to do so, 
then it would return the errors of *all* command signatures it tried to match against but failed. 
Instead, in case of failure, it simply says that a command has not been found. 

##### `Read[A]` ambiguities
A `Read[A]` ambiguity may occur when two or more `Read[A]` instances are 
capable of reading the same string input. In general, this is something that *buccaneer* 
actively avoids through the type system and additional syntactic measures. 
There is however one particular, very specific case where, due to its choice of supporting 
(almost) full generality when reading strings, such ambiguities may occur.
This is namely the case when a CLI can read both single values of type `A`, 
and one-element collections of type `A`. 
<br />
For example: 
```scala
val argInt = argument[Int]
val listInt = argument[List[Int]]

argInt("1") // success
listInt("1") // success
```
When reading a single `Int` string, both the `argInt` and `listInt` will happily 
consume that input. `argInt` will convert it to a single integer, whilst 
`listInt` will convert it to a list of one integer. The same goes for one-element maps and
assignments using `=`: 
```scala
val assignInt = assignment[Int]("a=")
val mapInt = argument[Map[String, Int]]

assignInt("a=5") // success
mapInt("a=5") // success
``` 
Should a command line application contain such cases: 
```scala
val cli = Cli (
(negate - argInt)(_ * -1),
(negate - listInt)(_ map(_ * -1)))

Interpreter.
  intrepret(cli).
  run(List("5")). // ambigous 
  print
```
Then the result of interpreting such an input will lead to ambiguity and thus an error.
The error will explicitly state which conversions came into conflict. 
<br />
<br />
And that's it. You can now create arbitrarily large and complex command
line interfaces. Before you start, you may however want to read the next
section. There are some additional interesting features that come for free. 

### MAN pages and input suggestions
Due to the way command signatures are modeled, *buccaneer* is able to generate MAN pages 
and offer command input suggestions dynamically and for free! 
##### MAN pages
Each primitive you use for composing commands may additionally receive a `String` description,
that is displayed in the MAN page representation of that command.
```scala
val add = command("add").msg("The add command")
val subtract = command("subtract").msg("The subtract command")
val int = argument[Int].msg("An integer argument")
vall double = argument[Double].msg("A double argument")

val interface = Cli(
  (add - int - int)(_ + _),
  (add - double - double)(_ - _),
  (subtract - int - int)(_ - _),
  (subtract - double - double)(_ - _))
```
In order to receive MAN page support, you need only use the interpreter that 
builds this feature in:
```scala
def runPrint(input: List[String]) = {
Interpreter.
  interpretHS(interface).
  run(input).
  print
}
```
`interpretHS` is the interpreter that supports both features. There are also interpreters that 
support each feature individually, namely `interpretH` (for MAN pages) and `interpretS` (for suggestions).
<br />
Now, any time a command input ends with `-help` or `--help` (you can change these), 
the interpreter will compile a MAN page using the command
signatures and then print it. 
<br />
<b>Note</b>: `-help` or `--help` can be called at any point during command input.
```scala
val help1 = List("add", "--help")
val help2 = List("add", "1", "--help")

runPrint(help1)
runPrint(help2)
```
Both of these will print a variation of the following: 
```bash
NAME
    add - The add command

USAGE
    add  [Int] [Double]

OPTIONS
    <Int> <Int>                       An integer argument
    <Double> <Double>                 A double argument
    
SUB-COMMANDS
    No sub-commands avaliable
```

In general , `--help` will return a page with the following template:
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

##### Input suggestions
Again, due to the way commands signatures are modeled, the system is able to support 
dynamic input suggestions. Any time a command input ends with `-sgst` or
`--sgst` (you can change these), the interpreter will use the command signatures to compile a list of suggestions that 
partially match that given input. 
<br />
<b>Note</b>: Similar to MAN pages, these can be called at any point during invocation:
```scala
runPrint(List("subtract","--sgst"))
```
Will print something along the lines of: 
```bash
<Int> <Int>
<Double> <Double>
```

Whilst:
```scala
runPrint(List("subtract", "1", "--sgst"))
```
Will narrow down accordingly and print:
```bash
<Int> <Int>
```

##### Configuration 
You are additionally given the possibility to configure the MAN page.
More concretely, you can provide the following information:
* **Application name**
  * This is a somewhat necessary requirement, as I do not really have direct access to how
  the application is called. Essentially it should be the `<name>` in `<name> add 1 2`.
  This gets printed in the MAN page.
* **Application description**
  * A description of the application. This gets printed next to the **Application name**.
* **Help**
  * You can change the options that trigger both the MAN page and input suggestions. 
    Because they are simply options, you define them analogously to how command
    options are defined. These however get passed to the configuration instead of the CLI itself.
    ```scala
    val myhelp = option("-h", "--help").msg("Prints this page.")
    ```
*  **Suggestions**
   * Analogous to **Help**:
   ```scala
   val mysuggest = option("-s", "--suggest").msg("Returns a list of input suggestions")
   ```
*  **Text width**
   * The overall line width per text block in a MAN page is alterable. Width is defined in terms of 
   the desired number of characters per line.

*  **Indentation**
   * The indentation per line is also configurable. Indentation is defined in terms of the desired
   number of empty characters at the beginning of indented lines.

*  **Column spacing**
   * The spacing used between parallel columns is alterable. Spacing is defined in terms of 
   the desired number of empty characters columns.   

**Example:**
```scala
val manconf = manpage(
  programName = "calculator",
  programDescription = "A simple calculator",
  help = myhelp,
  suggest = mysuggest,
  textWidth = 200,
  indentation = 10
  columnSpacing = 10)

def print(input: List[String]) = {
Interpreter.
    interpretHS(interface, manconf).
    run(input).
    print
}
```

For concrete examples, take a look at the [examples](../examples) package.
### Macros (sort-of)
The command interpreter is _very_ similar to how a programming language compiler is built.
It is a composition of a series of phases, that take the result of the previous phase, validate
and transform it in some way, and then pass it on. In *buccaneer*, each phase is actually
a pure function that is composed of other pure functions. More concretely, they are
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