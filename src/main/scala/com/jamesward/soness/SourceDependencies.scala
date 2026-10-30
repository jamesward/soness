package com.jamesward.soness

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * A project's source dependencies: other checkouts its build compiles from source (sbt's `RootProject(file(…))` and
 * `ProjectRef(file(…), …)`, Gradle's `includeBuild(…)`). Their code is part of the work as much as the project's is: a
 * change a request asks for may belong in one, though an IDE's tools reach only the project's own directory.
 */
object SourceDependencies:

  private val SbtRef = """(?:RootProject|ProjectRef)\s*\(\s*file\s*\(\s*"([^"]+)"\s*\)""".r
  private val GradleInclude = """includeBuild\s*\(?\s*['"]([^'"]+)['"]""".r

  /** The directories `text` (a build definition) names as source dependencies, as written. */
  def named(text: String): List[String] =
    (SbtRef.findAllMatchIn(text).map(_.group(1)) ++ GradleInclude.findAllMatchIn(text).map(_.group(1))).toList.distinct

  /** The source dependencies of the project at `root` that exist, resolved, in the order its build names them. */
  def of(root: Path): List[Path] =
    val project = root.resolve("project")
    val builds =
      List("build.sbt", "settings.gradle", "settings.gradle.kts").map(root.resolve) ++
        scala.util.Try(Files.list(root).iterator.asScala.filter(_.toString.endsWith(".sbt")).toList).getOrElse(Nil) ++
        scala.util.Try(Files.list(project).iterator.asScala.filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".sbt")).toList)
          .getOrElse(Nil)
    builds.distinct.filter(Files.isRegularFile(_)).flatMap(f => scala.util.Try(Files.readString(f, UTF_8)).toOption.toList.flatMap(named))
      .distinct.map(d => root.resolve(d).normalize).filter(d => Files.isDirectory(d) && !d.equals(root.normalize))

  /** The context line naming them, for the project's context. */
  def describe(dependencies: List[Path]): Option[String] =
    Option.when(dependencies.nonEmpty):
      "Source dependencies (other checkouts this project's build compiles from source; a change may belong in one, " +
        "made there with absolute paths): " + dependencies.mkString(", ")
