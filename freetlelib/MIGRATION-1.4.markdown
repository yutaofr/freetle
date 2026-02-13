FREETLE 1.4 MIGRATION GUIDE
===========================

This guide covers migration from legacy `Stream`/`null` APIs to the new `LazyList`/`Option` APIs introduced in the Scala 2.13 refresh.

The migration is compatibility-first:
- Legacy entry points are still available.
- New entry points are available now and are the preferred long-term APIs.
- Legacy entry points are marked deprecated where a direct replacement exists.

Scala/Toolchain baseline:
- Scala `2.13.17`
- Java `11`

Deprecation Map
---------------

| Area | Legacy API | Preferred API | Compatibility |
| --- | --- | --- | --- |
| Core stream type | `Stream`-based CPS streams | `LazyList`-based CPS streams | Legacy accepted via compatibility bridges |
| XML push DSL | `>(events: Stream[XMLEvent])` | `>(events: LazyList[XMLEvent])` | Legacy overload kept, deprecated |
| Translate push DSL | `>(events: Stream[XMLEvent])` and `>(ctx => Stream[XMLEvent])` | `>(events: LazyList[XMLEvent])` and `>(ctx => LazyList[XMLEvent])` | Legacy overloads kept, deprecated |
| XML char stream loader | `loadXMLResultStream(charStream: => Stream[Char])` | `loadXMLResultStream(charStream: => LazyList[Char])` | Legacy overload kept, deprecated |
| Translate char stream loader | `loadXMLResultStream(charStream: => Stream[Char])` | `loadXMLResultStream(charStream: => LazyList[Char])` | Legacy overload kept, deprecated |
| XML schema loader argument | `loadXMLResultStream(inputStream, xsdURL: String = null)` | `loadXMLResultStream(inputStream, xsdURL: Option[String])` | Legacy overload kept, deprecated |
| File splitter writer input | `serializeXMLResultStream(..., writerInput: Writer = null, ...)` | `serializeXMLResultStream(..., writerInput: Option[Writer], ...)` | Legacy overload kept, deprecated |
| Namespace-aware matching | `testElem(name, attributes)` | `testElem(name, attributes, namespaces)` | Legacy hook kept, deprecated |
| Namespace-aware context write | `pushToContext(name, attributes, context)` | `pushToContext(name, attributes, namespaces, context)` | Legacy hook kept, deprecated |

Code Examples
-------------

Legacy (still supported):

```scala
val events: Stream[XMLEvent] = Stream(EvText("hello"))
val pushed = >(events)

val loaded = XMLResultStreamUtils.loadXMLResultStream("<a/>".toStream)
val withXsd = XMLResultStreamUtils.loadXMLResultStream(inputStream, null)
```

Preferred:

```scala
val events: LazyList[XMLEvent] = LazyList(EvText("hello"))
val pushed = >(events)

val loaded = XMLResultStreamUtils.loadXMLResultStream("<a/>".to(LazyList))
val withXsd = XMLResultStreamUtils.loadXMLResultStream(inputStream, Option.empty[String])
```

Compatibility Test Matrix
-------------------------

Automated compatibility tests:
- `org.freetle.CPSXMLMigrationCompatibilityTest`
- `org.freetle.CPSTranslateMigrationCompatibilityTest`

These tests verify legacy and preferred signatures produce equivalent behavior for:
- XML char stream loading (`Stream` vs `LazyList`)
- Translate char stream loading (`Stream` vs `LazyList`)
- Legacy push overloads in XML/Translate models
- Legacy null/XSD input overload vs `Option[String]`

Build and runtime verification matrix:

| Command | Purpose | Expected |
| --- | --- | --- |
| `mvn -q -f freetlelib/pom.xml test` | Full library compile + tests | Pass |
| `mvn -q -f bootstrapxsd/pom.xml test` | Downstream module compatibility | Pass |
| `mvn -q -f freetlelib/pom.xml -Dtest=org.freetle.CPSStreamingVerifyTest -DargLine='-Xmx28m -Xss256k' test` | Streaming resilience (constrained heap) | Pass |
| `mvn -q -f freetlelib/pom.xml -Dtest=org.freetle.CPSStreamingVerifyTest -DargLine='-Xmx24m -Xss256k' test` | Lower-bound resilience probe | Known OOM boundary (same pattern as baseline) |
| `mvn -q -f freetlelib/pom.xml -Dtest=org.freetle.BenchmarkTest test` | Performance smoke benchmark | Pass, inspect printed timings |

Recommended Migration Sequence
------------------------------

1. Replace `Stream` usage with `LazyList` in your extension code.
2. Replace `null` optional parameters with `Option` wrappers.
3. Switch matcher/context hooks to namespace-aware signatures.
4. Remove reliance on deprecated overloads before the next major release.

Notes
-----

- Freetle keeps legacy APIs to protect existing integrations.
- New APIs are now the canonical path for Scala 2.13+ compatibility and future maintenance.
