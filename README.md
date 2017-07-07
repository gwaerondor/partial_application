# Partial Function Application in Erlang

Or at least something that behaves close to it...

Proper README coming soon<sup>TM</sup>.

## How does it work?
This is a parse transform.

If any argument of any function call is \_ (underscore), this will instead return a new function that only takes the missing parameters.

## No, how does it _really_ work?
The syntax tree is scanned for any _call_, and if either of the arguments of the function call is a variable with the name \_, this function call in the AST is replaced with a lambda function, which takes as many arguments as there were understores in the original "call" (which no longer is a call). The lambda function will replace all _ with placeholder names, and all the lambda does is to call the original function.

It also works for operations, which are represented differently from functions in Erlang. Operators are those infix thingies like +, -, rem, div and so on.

## Examples
```erlang
    lists:map(_ * 2, [1, 2, 3, 4]).
        [2, 4, 6, 8]

    Count_up = lists:seq(1, _),
    Count_up(10).
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

## Status
### Done
1. Functions can be "called" with missing arguments, and return a new function that requires only the missing ones.

### Not done
1. It isn't possible to do ```f(_, 2)(1)``` to call ```f(1, 2)``` (cannot think of any reason why you would ever do that, but then again I cannot think of any reason it shouldn't work, either).
2. It isn't possible to do ```fun(X) -> X end(_)``` (which is even sillier than the previous point but still _should_ be doable).

### Not working
1. There are shadowing problems if two partial applications are used in the same expression. For example ```_ + f(_ + 3)```. That will not compile at all.
2. ```badfun``` is generated for the following expression: ```Is_even = _ rem 2 == 0```. Not quite sure why yet, but there is no support for making two operations into one lambda. So that line will be something like ```fun(X) -> X rem 2 end == 0``` which makes no sense, while the user probably wanted ```fun(X) -> X rem 2 == 0 end```.
3. Probably tons of other stuff!
