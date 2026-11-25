package compiler

import javax.inject.Inject
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.model.ObjectFactory
import org.gradle.api.provider.Property
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.InputFile
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.OutputFile
import org.gradle.api.tasks.TaskAction

class NativeImageTask extends DefaultTask {
  enum Option {
    STATIC('--static'),
    MUSL('--libc=musl'),
    LINK_BUILD('--link-at-build-time'),
    G1GC('--gc=G1'),
    CPU('-march=native'),
    PGO('--pgo')

    final String arg
    private Option(String s) { this.arg = s }
  }

  @Input
  List parameters = [Option.STATIC, Option.MUSL, Option.CPU]

  @Input
  Integer minHeap = 1
  @Input
  Integer maxHeap = 32
  @Input
  Integer maxNew = 32

  @Input
  List<String> extraArgs = []

  private final RegularFileProperty shadowJarArchive
  private final Property<String> executableBaseName
  private final DirectoryProperty nativeWorkDir
  private final RegularFileProperty outputExecutable

  @Inject
  NativeImageTask(ObjectFactory objects) {
    shadowJarArchive = objects.fileProperty()
    executableBaseName = objects.property(String)
    nativeWorkDir = objects.directoryProperty()
    outputExecutable = objects.fileProperty()
    outputExecutable.set(
      nativeWorkDir.file(
        executableBaseName.map { baseName -> baseName }
      )
    )
  }

  @InputFile
  RegularFileProperty getShadowJarArchive() {
    shadowJarArchive
  }

  @Input
  Property<String> getExecutableBaseName() {
    executableBaseName
  }

  @Internal
  DirectoryProperty getNativeWorkDir() {
    nativeWorkDir
  }

  @OutputFile
  RegularFileProperty getOutputExecutable() {
    outputExecutable
  }

  @TaskAction
  void runCommand() {
    def nativeImagePath = findNativeImagePath()
    if (nativeImagePath == null) {
      throw new GradleException('native-image command not found on GRAALVM_HOME or PATH')
    }
    def heap = [
      "-R:MinHeapSize=${minHeap}m",
      "-R:MaxHeapSize=${maxHeap}m",
      "-R:MaxNewSize=${maxNew}m"
    ]
    def paramArgs = parameters.collect { it instanceof Option ? it.arg : it.toString() }
    def jarPath = shadowJarArchive.get().asFile.absolutePath
    def source = ['-jar', jarPath, '-o', executableBaseName.get()]
    def command = ([nativeImagePath, '--no-fallback'] + paramArgs + heap + extraArgs + source)
      .collect { it.toString() }
    logger.lifecycle "Executing native-image command -> '${command.join(' ')}'"

    def workDir = nativeWorkDir.get().asFile
    workDir.mkdirs()
    def process = new ProcessBuilder(command)
      .directory(workDir)
      .redirectErrorStream(true)
      .start()

    process.inputStream.withReader { reader ->
      reader.eachLine { line ->
        logger.lifecycle(line)
      }
    }

    def exitCode = process.waitFor()
    if (exitCode != 0) {
      logger.error "Unable to execute native-image: '${exitCode}'"
      throw new GradleException("native-image failed with exit code ${exitCode}")
    }
  }

  private String findNativeImagePath() {
    def graalvmHome = System.getenv('GRAALVM_HOME')
    if (graalvmHome) {
      def graalvmNativeImage = new File(graalvmHome, 'bin/native-image')
      if (graalvmNativeImage.canExecute()) {
        return graalvmNativeImage.absolutePath
      }
    }
    def pathValue = System.getenv('PATH') ?: ''
    for (def entry : pathValue.split(File.pathSeparator)) {
      if (!entry) {
        continue
      }
      def candidate = new File(entry, 'native-image')
      if (candidate.canExecute()) {
        return candidate.absolutePath
      }
    }
    null
  }
}

class NativeImagePlugin implements Plugin<Project> {
  @Override
  void apply(Project project) {
    def nativeTask = project.tasks.register('nativeImage', NativeImageTask) { t ->
      t.group = 'native'
      t.description = 'Builds a GraalVM native image from a JAR with a configured Main-Class'
      t.executableBaseName.set(project.name)
      t.nativeWorkDir.set(project.layout.buildDirectory)
    }
    project.plugins.withId('com.gradleup.shadow') {
      nativeTask.configure { t ->
        t.shadowJarArchive.set(project.tasks.named('shadowJar').flatMap { it.archiveFile })
        t.dependsOn(project.tasks.named('shadowJar'))
      }
    }
  }
}
