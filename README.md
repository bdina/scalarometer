# Scalarometer

A lightweight, thread-safe **Scala 3 LTS** metrics and telemetry library designed for JVM and GraalVM Native Image applications with zero third-party runtime dependencies beyond `scala3-library_3`.

## Features

- **Counters & Aggregates**: Lock-free [`Counter`](src/main/scala/com/scalarometer/statistics/Counter.scala) (`AtomicLong`) with bounded decrements, plus [`AggregateCounter`](src/main/scala/com/scalarometer/statistics/AggregateCounter.scala) tracking min, max, sample count, and average.
- **Gauges**: Functional [`Gauge`](src/main/scala/com/scalarometer/statistics/Gauge.scala) metrics supporting multi-key samplers and single-value factories (`Gauge.single`).
- **Histograms**: Bounded-memory [`Histogram`](src/main/scala/com/scalarometer/statistics/Histogram.scala) computing mean, variance, standard deviation, and linearly interpolated percentiles (`P_000` through `P_100`).
- **Scala Collections-Friendly Registry**: Thread-safe [`Registry`](src/main/scala/com/scalarometer/statistics/StatisticsRegistry.scala) implementing `Iterable[Metric]` with separate resettable and fixed metric sets (`+=`, `-=`, `map`, `filter`, `get`, `contains`, `stats(reset)`).
- **StopWatch**: Deterministic, thread-safe [`StopWatch`](src/main/scala/com/scalarometer/statistics/StopWatch.scala) with `Marker` support, injectable clock for testing, and `StopWatch.timed`.
- **Optional JMX Exposure**: Standard MBean [`ReaderBean`](src/main/scala/com/scalarometer/statistics/StatsReader.scala) (`com.scalarometer.statistics:type=ReaderBean`) gated behind `com.sun.management.jmxremote` and safe in GraalVM Native Image environments.
- **Zero External Runtime Dependencies**: Uses only `scala3-library_3` and `java.util.logging`.

## Requirements

- **Consumer Runtime**: Java `17+` (compiled with `-release:17`, classfile major version `61`) and **Scala `3.9.0` LTS** (TASTy forward-compatible with Scala `3.9+` and `3.10+`).
- **Development / Build**: `graalvm-dev:latest` Docker image (GraalVM JDK `25` + Gradle `9.9.2`), or JDK `25` + Gradle `9.x` locally.

## Quick Start

```scala
import com.scalarometer.statistics._

val registry = Registry()

val requests = Counter("http.requests")
val latency  = Histogram("http.latency.ms", max_snapshots = 1024)
val batch    = AggregateCounter("batch.size")
val uptime   = Counter("service.uptime.ticks")
val workers  = Gauge.single("workers.active")(java.lang.Integer.valueOf(8))

registry += requests += latency += batch += workers
registry.registerFixedCounter(uptime)

requests += 1L
latency.incrementByAndRecordSnapshot(12L)
batch.increment(25L)
uptime.increment()

val (result, elapsedMs) = StopWatch.timed {
  "ok"
}

// Read snapshot and atomically reset resettable metrics (fixed metrics keep accumulating)
val snapshot: Map[String, Number] = registry.stats(reset = true)

// Standard Scala collections integration via Iterable[Metric]
val metricNames: Set[String] = registry.map(_.name).toSet
```

### Optional JMX Registration

Enable JMX remote (`-Dcom.sun.management.jmxremote`) and register any `Registry`:

```scala
registry.initializeJmx()
// Exposes com.scalarometer.statistics:type=ReaderBean with getStatistics(Boolean)
registry.unregisterJmx()
```

## Building & Testing

All development and validation runs inside the `graalvm-dev:latest` Docker container via [`scripts/docker-build`](scripts/docker-build) (isolated `.gradle-docker/` and `.gradle-project-docker/` caches):

### Run Tests

```bash
./docker-build-test.sh
# or directly:
./scripts/docker-build gradle --no-daemon clean test --warning-mode all
```

### Build Library & Shadow JARs

```bash
# Thin library JAR (retains .tasty files for downstream Scala 3 projects) + sources JAR
./scripts/docker-build gradle --no-daemon jar sourcesJar

# Standalone fat JAR (classifier: all; excludes scala3-compiler)
./scripts/docker-build gradle --no-daemon shadowJar
```

### Publish to Local Maven Repository

```bash
./scripts/docker-build gradle --no-daemon publishToMavenLocal
```

Consuming from a downstream Gradle project (`mavenLocal()`):

```groovy
implementation "com.scalarometer:scalarometer:<version>"
```

### Interactive Container Shell / Scala 3 REPL

```bash
./scripts/docker-build
# inside container:
gradle repl
```

### Docker JVM Build & Test Image

```bash
docker build -f Dockerfile.jvm --tag scalarometer:latest .
```

## Documentation

- [Architecture Guide](docs/ARCHITECTURE.md) — Metric design, concurrency guarantees, serialization, JMX & GraalVM Native Image notes, and build/CI automation.

## License

See [LICENSE](LICENSE) for details.
