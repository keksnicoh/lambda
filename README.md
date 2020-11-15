# lambda

This is a playground repository which started by playing aroung with untyped lambda calculus. 
It was tempting to implement a simple language allowing to define free variables (as simple assignments) and provide functions as tools to inspect expressions. 
Functions support ad-hoc polymorphism, which can be archived by pattern matching the function domain.. boring. 
Thus, after several attempts, singletons ran accross the path allowing to lift a Haskell function into the weird language by mapping its type signature to a singleton identifying a sigma type. 
This allows to pick the right function depending on the given list of arguments.
Next experiment will by some jupyter like notebook api...
