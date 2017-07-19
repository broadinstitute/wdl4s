[![Build Status](https://travis-ci.org/broadinstitute/wdl4s.svg?branch=develop)](https://travis-ci.org/broadinstitute/wdl4s?branch=develop)
[![Coverage Status](https://coveralls.io/repos/broadinstitute/wdl4s/badge.svg?branch=develop)](https://coveralls.io/r/broadinstitute/wdl4s?branch=develop)

# Scala binding API for WDL

This repository provides scala tools to parse a [WDL](https://github.com/broadinstitute/wdl) file and transform it into a scala object hierarchy.

## Installation

wdl4s is hosted on The Broad Institute's [Artifactory Repository]()

```
resolvers ++= Seq(
  "Broad Artifactory Releases" at "https://broadinstitute.jfrog.io/broadinstitute/libs-release/"
)
```

Add the following to `libraryDependencies`:

```
"org.broadinstitute" %% "wdl4s" % version,
```

The latest version is ![Latest Version](version.png)

Or add a snapshot release in the format `<version>-<git-hash7>-SNAP`:

```
"org.broadinstitute" %% "wdl4s" % "0.123-fff0246-SNAP",
```


To use in your Maven project add the following dependency

```xml
<dependency>
    <groupId>org.broadinstitute</groupId>
    <artifactId>wdl4s_2.11</artifactId>
    <version>${version}</version>
</dependency>
```

## Scaladoc

[![Scaladoc](scaladoc.png)](http://broadinstitute.github.io/wdl4s/latest)

## Usage

All examples are located in `src/main/scala/wdl4s/examples` and particular examples can be run via `sbt`:

```
$ sbt "run-main wdl4s.examples.ex1"
```

### Loading WDL Code

The main entry point into the parser is the `WdlNamespace` object. A [WDL](https://github.com/broadinstitute/wdl) file is considered a namespace, and other namespaces can be included by using the `import` statement (but only with an `as` clause).

the [WdlNamespace](http://broadinstitute.github.io/wdl4s/latest/#wdl4s.WdlNamespace$) object has a few `load…()` functions for turning WDL source into `WdlNamespace` objects.

If the workflow being loaded contains a `workflow` definition, then the `load…()` functions will return a [WdlNamespaceWithWorkflow](http://broadinstitute.github.io/wdl4s/latest/#wdl4s.WdlNamespaceWithWorkflow) and otherwise they will return a [WdlNamespaceWithoutWorkflow](http://broadinstitute.github.io/wdl4s/latest/#wdl4s.WdlNamespaceWithoutWorkflow).

Example `src/main/scala/wdl4s/examples/ex1.scala`

```scala
val wdl = """
  |task a {
  |  command { ps }
  |}
  |workflow wf {
  | call a
  |}""".stripMargin

val ns = WdlNamespaceWithWorkflow.load(wdl, Seq.empty).get

println(s"Workflow: ${ns.workflow.unqualifiedName}")
ns.workflow.calls foreach {call =>
  println(s"Call: ${call.unqualifiedName}")
}

ns.tasks foreach {task =>
  println(s"Task: ${task.name}")
  println(s"Command: ${task.commandTemplate}")
}
```

## Using An Import Resolver

WDL code can have `import` statements but wdl4s does not know how to resolve these import statements into WDL source code.

When using `WdlNamespace.load()`, one can pass an optional import resolver which is a `String => WdlSource` function (`WdlSource` is a type alias for `String`).  If the import resolver cannot resolve the import string to WDL source, then it is expected to throw an exception.

Example `src/main/scala/wdl4s/examples/ex2.scala`

```scala
val wdl = """
  |import "some_string"
  |task a {
  |  command { ps }
  |}
  |workflow wf {
  | call a
  |}""".stripMargin

def resolver(importString: String): WdlSource = {
  importString match {
    case "some_string" => "task imported { command {ps} }"
    case s if s.startsWith("http://") =>
      // issue HTTP request
      throw new NotImplementedError("not implemented")
  }
}

val ns = WdlNamespaceWithWorkflow.load(wdl, Seq(resolver _)).get

ns.tasks foreach {task =>
  println(s"Task: ${task.name}")
}
```

Since the resolver is set up to resolve `some_string` to some static WDL code (`task imported { command {ps} }`, the output of this program will show two tasks in this namespace:

```
Task: a
Task: imported
```

WDL also supports `import "something" as namespace_name` format for imports.  In this case, a sub-namespace will be created where the tasks live

Example `src/main/scala/wdl4s/examples/ex3.scala`

```scala
val wdl = """
  |import "some_string" as my_namespace
  |task a {
  |  command { ps }
  |}
  |workflow wf {
  | call a
  |}""".stripMargin

def resolver(importString: String): WdlSource = {
  importString match {
    case "some_string" => "task imported { command {ps} }"
    case _ => throw new NotImplementedError()
  }
}

val ns = WdlNamespaceWithWorkflow.load(wdl, Seq(resolver _)).get

ns.tasks foreach {task =>
  println(s"Task: ${task.name}")
}

ns.namespaces foreach { n =>
  n.tasks.foreach { t =>
    println(s"Imported Task: ${t.name} (from ${n.importedAs})")
  }
}
```

Since the WDL `import` statement now has an `as` clause, the top-level namespace only has one task.  The top level namespace also has a sub-namespace called `my_namespace` which has one task.  The output of the program will be:

```
Task: a
Imported Task: imported (from my_namespace)
```

## Resolving Fully-Qualified Names

`WdlNamespace` has a `resolve` method which takes a fully-qualified name string and returns back the object that it refers to.

Example `src/main/scala/wdl4s/examples/ex4.scala`

```scala
val wdl = """
  |task a {
  |  command { ps }
  |}
  |workflow wf {
  | call a
  | call a as b
  |}""".stripMargin

val ns = WdlNamespaceWithWorkflow.load(wdl, Seq.empty).get

println(ns.resolve("wf.a")) // resolves to Call object for `call a`
println(ns.resolve("wf.b")) // resolves to Call object for `call a as b`
println(ns.findTask("a")) // resolves to Task object for `task a`
```

## Getting Dependencies

`Call` objects can have prerequisites: other `Call`s that need to be completed in order to start executing the `Call` in question.

Example `src/main/scala/wdl4s/examples/ex5.scala`

```scala
val wdl = """
  |task a {
  |  command { ps }
  |  output { File procs = stdout() }
  |}
  |
  |task b {
  |  File s
  |  command { wc -l ${s} }
  |}
  |
  |workflow wf {
  | call a
  | call b {input: s=a.procs}
  |}""".stripMargin

val ns = WdlNamespaceWithWorkflow.load(wdl, Seq.empty).get

Seq(ns.resolve("wf.a"), ns.resolve("wf.b")) foreach {
  case Some(c: TaskCall) => println(s"Call '${c.fullyQualifiedName}' prerequisites: ${c.upstream}")
  case _ =>
}
```

Since `call b` depends on `call a`, the set of prerequisites for `b` has one element and `a` has zero elements as seen from the standard output:

```
Call 'wf.a' prerequisites: Set()
Call 'wf.b' prerequisites: Set([Call name=a, task=[Task name=a commandTemplate=Vector( ps )}]])
```

## Evaluating Expressions

WDL has its own expression language.  The unevaluated expressions are stored in a `WdlExpression` object, which has an `evaluate()` method.

The `evaluate()` method takes two parameters: a lookup function (`String => WdlValue`) and an implementation of the standard library functions (see `trait WdlStandardLibraryFunctions`)

The lookup function is called for each variable that is encountered during expression evaluation.  The corresponding method in the standard library functions implementation is called for function invocations

Example `src/main/scala/wdl4s/examples/ex6.scala`

```scala
val wdl = """
  |workflow wf {
  |  String a = "foo" + "bar"
  |  String b = "hello " + variable
  |  String c = "hello " + other_variable
  |}""".stripMargin

val ns = WdlNamespaceWithWorkflow.load(wdl, Seq.empty).get
def lookup(name: String): WdlValue = {
  name match {
    case "variable" => WdlString("world")
    case _ => throw new NoSuchElementException
  }
}
ns.workflow.declarations foreach { decl =>
  val value = decl.expression.get.evaluate(lookup, NoFunctions)
  println(s"Declaration '${decl.toWdlString}' evaluates to: $value")
}
```

This evaluates each of the declarations in the workflow.  The last one fails because `other_variable` is not resolved with the `lookup` function that we defined

```
Declaration 'String a = "foo" + "bar"' evaluates to: Success(WdlString(foobar))
Declaration 'String b = "hello " + variable' evaluates to: Success(WdlString(hello world))
Declaration 'String c = "hello " + other_variable' evaluates to: Failure(java.util.NoSuchElementException)
```

## Instantiating Commands

Each `Task`'s command needs to be instantiated from the abstract form in the WDL file to a concrete form.  That means that each expression inside of `${...}` blocks needs to be evaluated.

Example `src/main/scala/wdl4s/examples/ex7.scala`

```scala
val wdl = """
  |task a {
  |  String prefix
  |  Array[Int] ints
  |  command {
  |    python script.py ${write_lines(ints)} > ${prefix + ".out"}
  |  }
  |}
  |workflow wf {
  |  call a
  |}""".stripMargin

val ns = WdlNamespaceWithWorkflow.load(wdl, Seq.empty).get
val inputs = Map(
  "prefix" -> WdlString("some_prefix"),
  "ints" -> WdlArray(WdlArrayType(WdlIntegerType), Seq(1,2,3,4,5).map(WdlInteger(_)))
)

class CustomFunctions extends WdlFunctions[WdlValue] {
  def write_lines(params: Seq[Try[WdlValue]]): Try[WdlValue] = {
    // Validate `params`, write the result to a file, return file path
    Success(WdlFile("/tmp/array.txt"))
  }
}

ns.taskCalls.find( _.unqualifiedName == "a") foreach { call =>
  val wdlFunctions: CustomFunctions = new CustomFunctions
  val evaluatedInputs = call.evaluateTaskInputs(inputs, wdlFunctions).get
  println(call.task.instantiateCommand(evaluatedInputs, wdlFunctions).get)
}
```

This will produce the following output:

```
python script.py /tmp/array.txt > some_prefix.out
```

## Accessing the WDL Parser Directly

To access only the parser, use the `AstTools` library, as follows:

Example `src/main/scala/wdl4s/examples/ex8.scala`

```scala
/* Create syntax tree from contents of file */
val ast = AstTools.getAst(Paths.get(args(0)))

/* Second parameter is a descriptor about where the first string came from.
 * Most of the time this would be the URI of where the text was loaded from,
 * but there are no restrictions on what the string can be.
 */
AstTools.getAst("workflow simple {}", "string")

/* Print the AST */
println(ast.toPrettyString)

/* Traverse the tree to find all Task definitions */
AstTools.findAsts(ast, "Task") foreach {ast =>
  println(s"Task name: ${ast.getAttribute("name").sourceString}")
}
```

