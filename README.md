# Scalarometer

A lightweight, high-throughput, thread-safe **Scala 3 LTS** metrics and telemetry library designed for JVM and GraalVM Native Image applications with zero third-party runtime dependencies beyond `scala3-library_3`.

## Features

- **Counters & Aggregates**: Lock-free [`Counter`](src/main/scala/com/scalarometer/statistics/Counter.scala) backed by an inlined `@volatile` `Long` and `@static` `VarHandle` (`@PolymorphicSignature`, zero wrapper object, overflow-safe `compareAndExchange` bounded decrements), plus [`AggregateCounter`](src/main/scala/com/scalarometer/statistics/AggregateCounter.scala) tracking min, max, sample count, and average with minimal lock hold times.
- **Gauges**: Functional [`Gauge`](src/main/scala/com/scalarometer/statistics/Gauge.scala) metrics supporting multi-key samplers, `Gauge.single`, and zero-boxing primitive SAM factories (`Gauge.long`, `Gauge.double`, `Gauge.int`).
- **Histograms**: Bounded-memory [`Histogram`](src/main/scala/com/scalarometer/statistics/Histogram.scala) backed by a lazily grown, reusable primitive `Array[Long]` buffer computing unboxed mean, variance, standard deviation, Scala 3 `IArray[Long]` snapshots (`snapshotValues`), and linearly interpolated percentiles (`P_000` through `P_100`, unboxed `percentileValue` and `Number` `percentile`).
- **Scala Collections-Friendly Registry**: Thread-safe [`Registry`](src/main/scala/com/scalarometer/statistics/StatisticsRegistry.scala) implementing `Iterable[Metric]` with separate resettable and fixed metric buckets (`+=`, `-=`, `++=`, `--=`, O(1) `get`/`contains`/`isEmpty`, allocation-free `iterator`/`foreach`, and single-pass `stats(reset)` via `Metric.appendData`).
- **StopWatch**: Deterministic, thread-safe [`StopWatch`](src/main/scala/com/scalarometer/statistics/StopWatch.scala) featuring zero-allocation Scala 3 `opaque type Marker` with extension methods (`.timestamp`, `.elapsed`, `.elapsedNanos`), injectable `LongSupplier` clock, and zero-overhead `inline` `StopWatch.timed` / `StopWatch.timedNanos`.
- **Optional JMX Exposure**: Standard MBean [`ReaderBean`](src/main/scala/com/scalarometer/statistics/StatsReader.scala) (`com.scalarometer.statistics:type=ReaderBean`) gated behind `com.sun.management.jmxremote` and safe in GraalVM Native Image environments.
- **Zero External Runtime Dependencies**: Uses only `scala3-library_3` and `java.util.logging` (cached via `ClassValue[Logger]` with Scala 3 `inline` level guards).

## Requirements

- **Consumer Runtime**: Java `17+` (compiled with `-release:17`, classfile major version `61`) and **Scala `3.9.0` LTS** (TASTy forward-compatible with Scala `3.9+` and `3.10+`, compiled with `-opt`, `-opt-inline:com.scalarometer.statistics.**`, and `-language:strictEquality`).
- **Development / Build**: `graalvm-dev:latest` Docker image (GraalVM JDK `25` + Gradle `9.9.2`), or JDK `25` + Gradle `9.x` locally.

## Quick Start

```scala
import com.scalarometer.statistics._

val registry = Registry()

val requests = Counter("http.requests")
val latency  = Histogram("http.latency.ms", max_snapshots = 1024)
val batch    = AggregateCounter("batch.size")
val uptime   = Counter("service.uptime.ticks")
val workers  = Gauge.int("workers.active")(8)

registry ++= List(requests, latency, batch, workers)
registry.registerFixedCounter(uptime)

requests += 1L
latency.incrementByAndRecordSnapshot(12L)
batch.increment(25L)
uptime.increment()

val p95: Double = latency.percentileValue(Histogram.Percentile.P_095)
val samples: IArray[Long] = latency.snapshotValues

val (result, elapsedMs) = StopWatch.timed {
  "ok"
}
val (_, elapsedNs) = StopWatch.timedNanos {
  "fast-path"
}

// Read snapshot in a single builder pass and atomically reset resettable metrics
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

## Migration Notes (Scala 3.9 Optimization Release)

Downstream Scala 3 consumers should recompile against the updated JAR/TASTy:

- `Counter` inlines its primitive `Long` counter using `@static` `VarHandle` instead of `protected val _count: AtomicLong` (use `value`, `increment`, `reset`, and `getAndResetValue()` in subclasses). Serialized metric classes bump `@SerialVersionUID` to `20261126010000L`.
- `StopWatch.Marker` is now a zero-allocation `opaque type Marker = Long` (source-compatible for `Marker(ts)`, `Marker.now`, `Marker.fromNanoTime`, `.timestamp`, and `StopWatch.elapsed(marker)`), and `StopWatch` accepts `java.util.function.LongSupplier` (Scala lambdas `() => expr` convert via SAM).
- `StopWatch.timed` and `StopWatch.timedNanos` are Scala 3 `inline` methods.
- `Gauge` is an `abstract class` instantiated via companion factories (`Gauge(name, f)`, `Gauge.single`, `Gauge.long`, `Gauge.double`, `Gauge.int`).
- `Histogram` exposes `percentileKey(p: Percentile)` in place of redundant per-percentile `protected val name000Percentile..name100Percentile` fields.

## Building & Testing

All development and validation runs inside the `graalvm-dev:latest` Docker container via [`scripts/docker-build`](scripts/docker-build) (persisted `.gradle-docker/caches/` + `.gradle-project-docker/`, with ephemeral daemon logs, `.tmp`, notifications, and `/tmp` mounted on RAM `tmpfs` to minimize host disk writes):

### Run Tests

```bash
# Incremental compile & test (fast default, minimal disk writes)
./docker-build-test.sh

# Full clean compile & test
./docker-build-test.sh --clean

# Optional HTML & JUnit XML test reports (disabled by default to minimize disk writes)
./docker-build-test.sh -PtestReports=true
```

### Build Library & Shadow JARs

```bash
# Thin library JAR (retains .tasty files for downstream Scala 3 projects) + sources JAR
./scripts/docker-build gradle --no-daemon jar sourcesJar

# Standalone fat JAR (classifier: all; excludes scala3-compiler and signing/maven metadata)
./scripts/docker-build gradle --no-daemon shadowJar
```

### Publish to Local Maven Repository (`~/.m2`)

Publishes `com.scalarometer:scalarometer_3:<version>` (standard Maven SemVer derived from `gitVersion()` with leading `v` stripped, e.g. `0.0.2`), thin library JAR (`.class` + `.tasty`), `-sources.jar`, `.pom`, and Gradle `.module` metadata directly into host `~/.m2/repository` with preserved host user ownership:

```bash
# Run tests and publish clean version to host ~/.m2/repository
./docker-publish-local.sh

# Publish a local SNAPSHOT or custom version (still requires clean git tree unless --allow-dirty)
./docker-publish-local.sh --version 0.0.3-SNAPSHOT

# Publish without re-running tests, or verify end-to-end in an isolated temp ~/.m2
./docker-publish-local.sh --skip-tests
./scripts/verify-maven-local
```

> Dirty working trees are blocked from publishing by default; pass `--allow-dirty` (or `-PallowDirtyPublish=true`) to override explicitly, or set `M2_DIR=/custom/path` to target an alternate Maven directory.

Consuming from a downstream Gradle project (`mavenLocal()`):

```groovy
repositories {
  mavenLocal {
    content {
      includeGroup 'com.scalarometer'
    }
  }
  mavenCentral()
}

dependencies {
  implementation "com.scalarometer:scalarometer_3:<version>"
}
```

When building a downstream project inside Docker (e.g., `graalvm-dev:latest`), mount the host repository read-only so `mavenLocal()` resolves published artifacts:

```bash
docker run ... -v "${HOME}/.m2:/root/.m2:ro" ...
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

- [Architecture Guide](docs/ARCHITECTURE.md) — Metric design, memory footprint & throughput architecture, concurrency guarantees, serialization, JMX & GraalVM Native Image notes, and build/CI automation.

## License

See [LICENSE](LICENSE) for details.
