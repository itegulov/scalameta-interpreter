Scalameta AST interpreter [![Build Status](https://travis-ci.org/itegulov/scalameta-interpreter.svg?branch=master)](https://travis-ci.org/itegulov/scalameta-interpreter)
====

Interpreter for [scalameta](http://scalameta.org/) syntax trees.

## CLI quick start

You can run scalameta-interpreter from the command line. Simply
download `org.scalameta.interpreter` jar file and launch it from
the command line:

```bash
scala -cp scalameta-interpreter.jar org.scalameta.interpreter.Main A.scala
```

This will interpret file `A.scala` and print result
to the command line.

## Major issues

Unfortunately, scalameta-interpreter needs a semantic information
which scalameta can not provide. So you need to annotate your code
with semantic information before scalameta-interpreter can interpret
it. Example is available
[here](https://github.com/itegulov/scalameta-interpreter/blob/master/interpreter/src/main/scala/org/scalameta/interpreter/ScalametaMirror.scala#L28).