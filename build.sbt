
name := "Disitaco"

version := "1.0"

scalaVersion := "3.7.1"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-encoding","ISO-8859-1",
  "-deprecation"
)

javacOptions ++= Seq("--release", "17")

// Generates Version.scala
Compile / sourceGenerators += Def.task {
  val file = (Compile / scalaSource).value / "ucesoft" / "disitaco" / "Version.scala"
  println(s"Generating Version.scala $file")
  IO.write(file,
    s"""package ucesoft.disitaco
       |object Version {
       | val VERSION = "${version.value}"
       | val SCALA_VERSION = "${scalaVersion.value}"
       | val BUILD_DATE = "${java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))}"
       |}
       |""".stripMargin)
  Seq(file)
}.taskValue

libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.6.0"
libraryDependencies += "com.formdev" % "flatlaf" % "3.6"
//libraryDependencies += "org.yaml" % "snakeyaml" % "2.3"
//libraryDependencies += "org.jfree" % "jfreechart" % "1.5.6"
//libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.19.0"

Compile / resourceDirectory := baseDirectory.value / "resources"
Compile / scalaSource := baseDirectory.value / "src"

val buildsDisitacoDist = taskKey[Unit]("build distribution zip file for ScalaPC")

buildsDisitacoDist := {
  def walk(file:File): List[File] = {
    if (file.isDirectory) {
      val files = for(f <- file.listFiles) yield walk(f)
      files.flatten.toList
    }
    else List(file)
  }

  val classDir: File = (Compile / classDirectory).value
  val packDir = classDir.getParentFile / "pack"
  IO.delete(packDir)
  IO.createDirectory(packDir)
  val packConfig = packDir / "config"
  val packRom = packDir / "rom"
  val packDisk = packDir / "disk"
  val packLib = packDir / "lib"
  IO.createDirectories(Seq(packLib))

  // copy images
  val resDir = (Compile / classDirectory).value / "resources"
  IO.copyDirectory(baseDirectory.value / "resources" / "resources",resDir)

  val files = walk(classDir)
  val jarFiles = files.map { f =>
    (f,f.toString.substring(classDir.toString.length + 1))
  }

  val disitaco = packLib / "disitaco.jar"
  IO.jar(jarFiles,disitaco,new java.util.jar.Manifest,None)

  val libraries = (Compile / managedClasspath).value.map(_.data) ++ (Compile / unmanagedClasspath).value.map(_.data)
  val scripts = (baseDirectory.value / "bin").listFiles.filter { f =>
    val name = f.getName.toUpperCase
    name.endsWith(".SH") || name.endsWith(".BAT")
  }
  val configs = (baseDirectory.value / "config").listFiles
  // copy config
  for (config <- configs) {
    IO.copyFile(config,packConfig / config.getName)
  }
  // copy libraries
  for (lib <- libraries) {
    IO.copyFile(lib,packLib / lib.getName)
  }
  val roms = (baseDirectory.value / "rom").listFiles
  // copy roms
  for (rom <- roms) {
    IO.copyFile(rom,packRom / rom.getName)
  }
  // copy disks
  val disks = (baseDirectory.value / "disk").listFiles
  for (disk <- disks)
    if (disk.getName.endsWith(".zip"))
      IO.unzip(disk,packDisk)
    else
      IO.copyFile(disk,packDisk / disk.getName)
  // copy scripts
  val libJarOnly = (libraries ++ Vector(disitaco)).filter(_.getName.endsWith(".jar")).map(_.getName)
  for(sc <- scripts) {
    val lines = IO.readLines(sc)
    val isLinuxShell = sc.getName.toUpperCase.endsWith(".SH")
    val newLine = if (isLinuxShell) 10.toChar.toString else 13.toChar.toString + 10.toChar
    val pathSep = if (isLinuxShell) ":" else ";"
    val dirSep = if (isLinuxShell) "/" else "\\"
    val libEnv = if (isLinuxShell) "$LIB" else "%LIB%"

    val linesWithCP = lines.map { line =>
      val cpLine = if (isLinuxShell) "CP=" else "set CP="
      if (line.startsWith(cpLine)) {
        val cp = libJarOnly.map(jar => s"$libEnv$dirSep$jar").mkString(pathSep)
        s"$cpLine$cp"
      }
      else line
    }
    val content = linesWithCP.mkString(newLine)
    IO.write(packDir / sc.getName,content)
  }

  // zip distribution
  val dist = baseDirectory.value / "dist"
  val zipFile = dist / s"Disitaco_${java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))}_install.zip"
  val zipFileSet = walk(packDir)
  val zipFiles = zipFileSet.map { f =>
    (f,"disitaco/" + f.toString.substring(packDir.toString.length + 1))
  }
  IO.zip(zipFiles,zipFile,None)
  // set permissions
  val fs = java.nio.file.FileSystems.newFileSystem(zipFile.toPath)
  val root = fs.getPath("/disitaco")
  val perm = java.nio.file.attribute.PosixFilePermissions.fromString("r-xr-xr-x")
  java.nio.file.Files.list(root).filter(p => p.toString.endsWith(".sh")).forEach(p => {
    java.nio.file.Files.setAttribute(p,"zip:permissions",perm)
  })
  fs.close()
  IO.copyFile(disitaco,dist / disitaco.getName)

  // clean pack dir
  IO.delete(packDir)
}