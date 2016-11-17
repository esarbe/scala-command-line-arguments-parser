=Command line parser with Scala=

First of all; if you are looking for a command line parser in Scala; this is not the library you are looking for.
If you are looking for a production-ready command line parser for/in Scala, try the
  [scopt library|https://github.com/scopt/scopt] or
  [scala-optparse-applicative | https://github.com/bmjames/scala-optparse-applicative].

This is first and foremost a learning project and not intended for productive (or hobby) use. In it, i will try to
build a nice command line parser in Scala, first using simple and naïve approaches and then - as I add more
features - more and more sophisticated ones. Since this is a learning project, I'll refrain from using libraries for
the most part. Only if I notice that I start duplicating large parts of a library will I resort to include it in the
project.

==The problem==
Everone using command line applications has used them; arguments. Those funny additional letters and word, often
(but not always) accompanied by hyphens, that controll the runtime behaviour of so many of our favorite applications.
Git has them, ls has them, only ```exit``` seems to be able to exist without their comforting promise of variation.

There is no common format for command line arguments, however. There are are libraries for all programming languages
- I bet there's even one for Visual Basic - but all of them vary in subtle ways. And since each application with command
line arguments has different argument names and different payload formats, each command line argument implementation
forms a separate little language. So, a command line parser library is not actually implementing a specific syntax,
rather is allows the definition of a (configuration) language.


```
L   -->
L   --> ARG
ARG --> COMMAND
ARG --> PARAM
ARG -->

```