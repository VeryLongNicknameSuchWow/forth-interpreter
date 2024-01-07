# Useful langugage extensions
The default wordlist is pretty minimal, so here are some useful word definitions and language extensions.

### Adding extra parentheses
```
"\"\\"\" 1 roll + \"\\"\" +" "wrap" define
```
The 'wrap' word encloses the top element of the stack in quotes, effectively wrapping it in parentheses. This is particularly useful for avoiding excessive nesting of parentheses in inline strings, such as when defining functions using conditionals.

```
"some word" wrap .p cr
```
```
"some word"
```
Notice that the printed parentheses are a part of the created string.

### Logic
Suppose you want to apply Boolean algebra between two terms:
```
"a" "b" = "c" "d" = and
```
this would correspond to `('a' == 'b' && 'c' == 'd')` in C-like notation.

Such `and` word can be simply defined as `*`, since `0` is evaluated as false and `1` is evaluated as true. Similarly `or` would be `+`.

You have to be careful though, since `*` operator is not defined for strings and will not work for combining their logical evaluations.

### More comparison operators
By default only `>` and `=` are available.

The `<` operator can be easily defined by reversing the order of operands in the `>` comparison:
```
"1 roll >" "<" define
```

`>=` can be added by applying both `>` and `=` like so:
```
"1 pick 1 pick > 2 roll 2 roll = +" ">=" define
```
As mentioned earlier, the `+` operator can function as a logical 'OR' in this context.

### Loops
Since loops are just a mere imperative caricature of recursion there are no loops in this language.

However, you can achieve behavior similar to a regular 'while' loop (and by extension, a 'for' loop) like so:
```
"i 1 + \"i\" define" "i++" define
"\"i i++ loop\" \".s bye\" i n = if" "loop" define

0 "i" define
10 "n" define
loop
```
```
[Integer 0,Integer 1,Integer 2,Integer 3,Integer 4,Integer 5,Integer 6,Integer 7,Integer 8,Integer 9]
```