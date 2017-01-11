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

There is no common format for command line arguments, however. There are are command line parsing libraries for all of mankind's
programming languages - I bet there's even one for Visual Basic - but all of them vary in subtle ways. And since each
application with command  line arguments has different argument names and different payload formats, each command line
argument implementation forms a separate little language. So, a command line parser library is not actually implementing
a specific language, rather it provides the tools for the the definition of a (configuration) language.


```
L     ::= flags | flag | command | arguments
command ::= command-names
flags ::= `-` flag

```


## The naive approach ##
Before trying to be clever, let's try something simple: A traditional, class based approach where each class is
responsible for one type of element in our language. 
The central element of our approach is the method 
``` def consume(args: Seq[String]): Either[Error, (Seq[String], T)] ```.
It's purpose is to receive a sequence of strings, consume as many elements of the sequence it can and then
return the remaining sequence, together with a object representing the result. In the error case, an error object
is returned instead.

By using combinator parser -  ```and``` along with ```or``` - we can build up a tree of parsers that then will consume
the whole sequence of strings if the parsing is successful. If anything remains to be consumed, this is an error.

This first naive approach works surprisingly well. Although the individual parsers are messy and complicated,
they compose well and it's possible to build complex parsers that consume flags, condensed flags, arguments and commands.
 Even error reporting works well, making it possible to generate nice error messages that are useful to the user.
  
What doesn't work is help. Especially nested help. We can handle a help request as just another error state,
but this approach further complicates the individual parsers.

## The monadic approach