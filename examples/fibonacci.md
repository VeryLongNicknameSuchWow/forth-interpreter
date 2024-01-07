# Fibonacci number calculation

## Stack-based approach
```
0 1 0
120 "n" define
"1 roll 0 pick 3 pick + 2 roll 1 +" "fib" define
"0 pick \"th fibonacci number is \" 4 pick + + .p cr" "print" define
"\"fib nFib\" \"print\" 2 pick n = if" "nFib" define
nFib
```

### Initial stack state
```
0 1 0
```

In this approach the stack is initialized with `0 1 0`, representing the first two numbers of the Fibonacci sequence (0 and 1) and a loop variable (also 0). This initial stack can be conceptualized as $[F_i, F_{i+1}, i]$ where $F_i$ and $F_{i+1}$ are two consecutive Fibonacci numbers and $i$ is their index in the sequence.

### Loop bound definition
```
120 "n" define
```

`n` is defined as 120, which specifies the number of iterations to perform in the loop.

### `fib` word definition
```
"1 roll 0 pick 3 pick + 2 roll 1 +" "fib" define
```
The `fib` word is defined to calculate the next Fibonacci number. It performs the following operations:
* The value of $F_{i+1}$ is moved to the top of the stack (`1 roll`).
* The top value of the stack ($F_{i+1}$) is duplicated (`0 pick`).
* The value of $F_i$ is copied to the top of the stack (`3 pick`).
* The two top stack elements ($F_i$ and $F_{i+1}$) are added together (`+`) creating $F_{i+2}$.
* The value of $i$ is moved to the top of the stack (`2 roll`) and incremented by one (`1 +`) creating $i+1$.

After those operations the stack will be in the state $[F_i, F_{i+1}, F_{i + 2}, i + 1]$. The next Fibonacci number has been calculated and `fib` word could be applied again.

### `print` word definition
```
"0 pick \"th fibonacci number is \" 4 pick + + .p cr" "print" define
```
The `print` word is defined to perform the following operations:
* The value of $i$ is copied to the top of the stack (`0 pick`).
* The string `"th fibonacci number is "` is pushed on top of the stack.
* The value of $F_i$ is copied to the top of the stack (`4 pick`).
* The two numbers and the string are concatenated together and printed (`+ + .p cr`).

This word doesn't change the state of the stack, the new elements are consumed for printing.

### `nFib` word definition
```
"\"fib nFib\" \"print\" 2 pick n = if" "nFib" define
```
The `nFib` word is defined to perform the following operations:
* Push the string `"fib nFib"` onto the stack to be executed if the condition is false (this introduces recursion).
* Add the string to be executed if the condition is true onto the top of the stack (`"print"`).
* Copy the value of $i$ to the top of the stack (`2 pick`).
* Put the value of `n` on top of the stack.
* Check whether the top two values of stack are equal and execute the corresponding string (`= if`).

This word also doesn't change the state of the stack.

### Program execution
```
nFib
```
Finally, executing the `nFib` word calculates the Fibonacci numbers up to the 120th term, as specified by `n`, and produces the output:
```
120th fibonacci number is 5358359254990966640871840
```

The `.s` command at this point would show that the stack contains all consecutive Fibonacci numbers from $F_0$ to $F_{121}$, along with the final index value of 120, reflecting the number of iterations set by `n`.

With a modification to the `fib` word, such as by adjusting the stack operations, you could streamline the stack to store only the two most recent Fibonacci numbers at any given time.

## Dictionary-based approach
```
0 "a" define
1 "b" define
"a b + b \"a\" define \"b\" define" "fib" define
"\"A=\" a + \", B=\" b + + .p cr" "print" define
```
This is a program which uses the dictionary to store data instead of the stack.

### Initital definitions
```
0 "a" define
1 "b" define
```
In this initial setup, `a` is defined as `0` and `b` as `1`, setting the starting points for the Fibonacci sequence calculation.

### `fib` word definiton
```
"a b + b \"a\" define \"b\" define" "fib" define
```
The `fib` word recalculates the values of `a` and `b` to generate the next Fibonacci number. The operations proceed as follows:

* The sum of $a$ and $b$ (representing the next Fibonacci number) is calculated and pushed onto the stack (`a b +`).
* $b$ is pushed on top of the stack (`b`)
* The value of `a` is then updated to the current value of `b`, and `b` is updated to the newly calculated sum, effectively shifting the Fibonacci sequence forward (`"a" define` and `"b" define`).

The words `a` and `b` would so get redefined with new values. Those words can thus be used like global variables of many programming languages. 

In this approach, the stack is utilized primarily for temporary data manipulation during calculations, while the dictionary serves as the main storage for the sequence's values, functioning akin to global variables in many programming languages.