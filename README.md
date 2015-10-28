# yarpn -- Yet another RPN Compiler and Interpreter in Haskell

This is a toy project that is loosely based on a Scala project that can be found here: [RPN Compiler and Interpreter](https://github.com/davidledwards/rpn)  


##Overview
This project contains a compiler and an interpreter that processes simple infix expressions and produces a list of postfix instructions that the interpreter can process and evaluate to produce a result.  
This is an learning project for me. It does not use a parsing library like Parsec.  
I found that this series of blog articles was a good introduction to parsing concepts: [Symbolic Calculator Recursion] (https://www.fpcomplete.com/user/bartosz/basics-of-haskell/4-symbolic-calculator-recursion)

##Building
You can build this project with either Stack or Cabal.

##Grammar
The grammar that is recognized by the compiler is as follows:

```
p0 ::= <p2> <p1>
p1 ::= '+' <p2> <p1>
   ::= '-' <p2> <p1>
   ::= '=' <p2> <p1>
   ::= e
p2 ::= <p4> <p3>
p3 ::= '*' <p4> <p3>
   ::= '/' <p4> <p3>
   ::= '%' <p4> <p3>
   ::= '^' <p4> <p3>
   ::= e
p4 ::= <p6> <p5>
p5 ::= 'min' <p6> <p5>
   ::= 'max' <p6> <p5>
   ::= e
p6 ::= '(' <p0> ')'
   ::= <symbol>
   ::= <number>
```

##Components
###yaprn-lib
This is a libary that contains the following:

* Tokenizer -- Converts a string into a list of tokens
* Parser -- Converts a list of tokens into a syntax tree
* Generator -- Generates a list of instructions from a syntax tree
* Evaluator -- Evaluates a list of instructions and produces a result

###yarpnc
This is the compiler. 
`Usage: yarpnc <input file> <option>`  
If an input file is provided it will be used for input.
If an input file is not provided then stdin is used for input.
Output is sent to stdout

The compiler has the following options:
* -t display the list of tokens
* -p display the parsed the syntax tree
* by default the compiler will emit a list of instructions to stdout

You can enter multiple assignment expressions.
You can enter one non-assignment expression.
An example is as follows:  
yarpnc   
x = 1  
y = 1  
x + y  
... output is generated

After the last expression is entered the compiler generates output based on the selected option.
###yarpni
This is the interpreter.  It will accept input from stdin or from a file that contains a list of instructions; one instruction per line.
`Usage: yarpni <file name>   

###Compiler and Interpreter Combined
The compiler and interpreter can be composed into an evaluator.  
Example: 
`yarpnc | yarpni`  


##Instructions
### `Sym <symbol> `  
Declares a variable that will be bound by the value provided by an assignment statement
### `Push <number>`  
Pushes a number on to the evaluation stack.
### `Movesym <symbol>`
Binds value to symbol by popping value off stack and adding symbol to symbol table.
### `Pushsym <symbol?`
Pushes symbol from symbol table onto the stack.  The symbol remains in the symbol table.
### `Add <number of arguments>`
Pop the number of arguments off the stack and add them; then push the value back onto the stack.
### `Min <number of arguments>`
Pop the number of arguments off the stack and push the mininum value back onto the stack.
### `Max <number of arguments>`
Pop the number of arguments off the stack and push the maximum value back onto the stack.
### `Pow 2`
Pop two values  off the stack and raise the first value by the value of the second; then push the result back onto the stack.
### `Mod 2`
Pop two values off the stack and return the modulus value back onto the stack.
### `Mul <number of arguments>`
Pop the number of arguments off the stack and return the product to the stack.
### `Sub <number of arguments>`
Pop the number of arguments off the stack, compute the difference and push value back on stack.

#Caveats
The exception handling and error handling are rudimentary.
The instruction emitter and loader are rudimentary. The compiler uses the Show typeclass and intrepreter uses Read typeclass.



#License
The MIT License (MIT)

Copyright (c) 2015 Dan Plubell 

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
