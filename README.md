# Command line parser with Scala

First of all; if you are looking for a command line parser in Scala; this is not the library you are looking for.
If you are looking for a production-ready command line parser for/in Scala, try the
  [scopt library](https://github.com/scopt/scopt) or
  [scala-optparse-applicative](https://github.com/bmjames/scala-optparse-applicative).

This is first and foremost a learning project and not intended for productive (or hobby) use. In it, i will try to
build a nice command line parser in Scala, first using simple and naïve approaches and then - as I add more
features - more and more sophisticated ones. Since this is a learning project, I'll refrain from using libraries for
the most part. Only if I notice that I start duplicating large parts of a library will I resort to include it in the
project.

The end-product - if there will ever be such a thing - should be a nice, compact and versatile Scala library for command
line arguments parsing.

## The problem
Everyone using command line applications has used them; arguments. Those funny additional letters and word, often
(but not always) accompanied by hyphens, that control the runtime behaviour of so many of our favorite applications.
Git has them, ls has them, only ```exit```, ```true``` and ```false``` seems to be able to exist without
their comforting promise of variation.

There is no common format for command line arguments, however. Though there are are libraries for all of mankind's
programming languages - I bet there's even one for Visual Basic - but all of them vary in subtle ways. And since each
application with command  line arguments has different argument names and different payload formats, each command line
argument implementation forms a separate little language. So, a command line parser library is not actually implementing
a specific syntax, rather it provides the tools for the the definition of a (configuration) language.

```
L     ::= flags | flag | command | arguments
command ::= command-names
flags ::= `-` flag

```



## The native approach
Before trying to be clever, let's try something simple: A traditional, class based approach whe

## The monadic approach