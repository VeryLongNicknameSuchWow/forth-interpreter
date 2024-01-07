# Documentation
## The Stack
Anything you enter inside the query is either a String, an Integer, or a Word. Query elements can be separated by spaces. Strings and Integers are added onto the stack upon evaluation. Words are executed instead. Each query is evaluated left to right.

### Data Types
The stack can contain either Strings or Integers.

You can push a string onto the stack by entering it with parentheses.
```
"This is a string" "You can use \" to escape parenthesis inside a String"
```

Integers can be either positive or negative. This type represents the entire infinite range of integers.

```
420 -2137 5358359254990966640871840
```

### Stack operations
You can use `.s` to print out the entirety of the stack. 
```
"foo" 420 .s
```
```
[String "foo",Integer 420]
```

To pop the top element from the stack `.` can be used.
```
"foo" 420 .
```
```
Integer 420
```

You can also remove the top element without printing it using `drop`.
```
"foo" 420 drop
```
The above query results in no output.

You can use `size` to add the current size of the stack onto the stack.
```
1 2 3 4 5 size .s
```
```
[Integer 1,Integer 2,Integer 3,Integer 4,Integer 5,Integer 5]
```

To access lower elements on the stack `pick` and `roll` words can be used. Those words will firstly get the number of elements to skip from the top of the stack and then either copy (`pick`) or move (`roll`) the next element on top of the stack.
```
1 2 3 4 5
3 pick .s
```
```
[Integer 1,Integer 2,Integer 3,Integer 4,Integer 5,Integer 2]
```
and
```
1 2 3 4 5
3 roll .s
```
```
[Integer 1,Integer 3,Integer 4,Integer 5,Integer 2]
```

## I/O
To print the top element from the stack `.p` word can be used. This will pop the top element and add its value to the output buffer.
Word `cr` can then be used to print the output buffer with a newline at the end.
```
"Hello world!" .p cr
```
```
Hello world!
```

## Data manipulation
### Arithmetics
Arithmetic operations work by manipulating data on the stack.
```
5 4 + 6 7 * 2200 63 - 24 12 / 7 2 mod .s
```
```
[Integer 9,Integer 42,Integer 2137,Integer 2,Integer 1]
```
This follows the Reverse Polish Notation (RPN) style.

The `+` operator can also be used to combine different data types together.
```
"The number " 42 " is the answer to The Ultimate Question" .s + + .s
```
```
[String "The number ",Integer 42,String " is the answer to The Ultimate Question"]
[String "The number 42 is the answer to The Ultimate Question"]
```

### Comparisons
For equality you can use `=` operator. It will add `1` onto the stack if two popped items from the stack are equal and `0` otherwise.
```
21 21 = "c" "d" = .s
```
```
[Integer 1,Integer 0]
```
Similarly, `>` can be used to check whether the top item is less than the second from top.
```
20 10 > 10 20 > .s
```
```
[Integer 1,Integer 0]
```

## Data execution and conditionals
Any item from top of the stack can be executed like a query using `exec`. This word will pop the top stack element and try to execute it.
```
"\"Hello world!\" .p cr" exec
```
```
Hello world!
```
Integers can be executed similarly - they simply get added onto the stack.
```
5 exec .s
```
```
[Integer 5]
```

### Conditionals
The word 'if' pops the item from the top of the stack, followed by two instructions for conditional execution.
```
"This will execute if popped nonzero integer or nonempty string" "This will execute otherwise" 1 if
```
For example:
```
"\"20 is not more than 10\"" "\"20 is more than 10\"" 20 10 > if .s
```
```
[String "20 is more than 10"]
```
Another example:
```
"\"10 is not more than 20\"" "\"10 is more than 20\"" 10 20 > if .s
```
```
[String "10 is not more than 20"]
```
And another example:
```
"\"Empty\"" "\"Not empty\"" "" if .s
```
```
[String "Empty"]
```
Notice how in the above examples escaped quotations are used inside quotations to add a String onto the stack instead of directly executing it.

## Language extensions
### Defining new words
Aside from default words (which can be listed with `words` or `debug`) the programmer is free to define new words. This can be done using the `define` word.
```
"\"Hello world!\" .p cr" "printHello" define
printHello
```
```
Hello world!
```
This allows the language to be extended and redefined during runtime.

### Removing words
Both the default and runtime-defined words can be unregistered using `undefine` word.

For example we can remove the previously defined `printHello` function like this:
```
"printHello" undefine
printHello
```
```
Runtime error: Word 'printHello' not in dictionary
```

### Storing data off-stack
You can also define and redefine constant functions similar to variables.
```
42 "answer" define
answer
"\"42 is the answer\"" "answer" define
answer
.s
```
```
[Integer 42,String "42 is the answer"]
```
This allows any data to be stored as a word definition.