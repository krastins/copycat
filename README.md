**copy·cat** */ˈkɒpikæt/* **1** one who imitates or adopts the behavior or practices of another

Markov chain based melody generator.

### Prerequisites

* Java 8+
* sbt
* ScalaMIDI library (it's not officially updated for Scala 2.13, [but there's a working unpublished fork](https://github.com/krastins/ScalaMIDI/tree/scala-2.13), you have to clone the branch and run `sbt publishLocal`)

### Development

Tests can be run with `sbt test`.

Build an uber jar with `sbt assembly`.

### Usage

You can run the program with sbt. It expects one argument, which is the path to the input file:

```sbt "run src/test/resources/midi/nokia.mid"```


Or use the compiled jar (if you have compiled it with `sbt assembly`) like this:

```java -jar target/scala-2.13/copycat-assembly-1.0.jar src/test/resources/midi/nokia.mid```