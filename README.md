# Partial Function Application in Erlang

Or at least something that behaves close to it...

Proper README coming soon<sup>TM</sup>.

## Done
1. Functions can be "called" with missing arguments, and return a new function that requires only the missing ones.

## Not done
1. Operators such as +, - or rem cannot be partially applied yet.
2. It isn't possible to do ```f(_, 2)(1)``` to call ```f(1, 2)``` (cannot think of any reason why you would ever do that, but then again I cannot think of any reason it shouldn't work, either).
3. It isn't possible to do ```fun(X) -> X end(_)``` (which is even sillier than the previous point but still _should_ be doable).

