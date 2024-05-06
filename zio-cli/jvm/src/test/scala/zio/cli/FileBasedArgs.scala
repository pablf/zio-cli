import zio._
import zio.test._
import zio.test.Assertion._
import java.nio.file.{Files, Paths, Path}
import java.io.IOException
import zio.cli._

object FileBasedArgs extends ZIOSpecDefault {

  val configFileOps: ConfigFilePlatformSpecific = ConfigFileArgsPlatformSpecific

  def spec = suite("FileBasedArgs")(
    test("should load options from files and merge them appropriatly") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        homeDir <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.home")))
        command <- ZIO.succeed("testApp")
        _       <- createSampleConfigFiles(cwd, homeDir, command)

        // Check if the func checkAndGetOptionsFilePaths can
        configArgs <- configFileOps.loadOptionsFromConfigFiles(command)

        _ <- cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command)

      } yield assert(configArgs)(hasSameElements(List("home=true", "dir=true", "home=false")))
    },
    test("should return directory ~/home and ./ which have .testApp config file for loading the args") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        homeDir <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.home")))
        command <- ZIO.succeed("testApp1")
        _       <- createSampleConfigFiles(cwd, homeDir, command)

        // Check if the func checkAndGetOptionsFilePaths can
        paths <- configFileOps.findPathsOfCliConfigFiles(command)

        _ <- cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command)

      } yield assert(paths)(hasSameElements(List(homeDir.toString(), cwd.toString())))
    },
    test("failing test 1") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        _       <- createSampleConfigFiles2(cwd, "someCommand")
        cliApp = CliApp.make(
          name = "cliApp",
          version = "0",
          summary = HelpDoc.Span.empty,
          command = Command("someCommand", Options.text("opt")),
        ) {
          case text: String => ZIO.succeed(text)
        }
        res <- cliApp.run(List("someCommand", "--opt", "inputText"))

        // Check if the func checkAndGetOptionsFilePaths can

        _ <- cleanUpSampleConfigFiles2(cwd, "someCommand")

      } yield assertTrue(res == Some("inputText"))
    },
    test("failing test 2") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        command = "someCommandB"
        _       <- createSampleConfigFiles2(cwd, command)
        cliApp = CliApp.make(
          name = "cliApp",
          version = "0",
          summary = HelpDoc.Span.empty,
          command = Command(command, Options.text("opt"), Args.integer("num")),
        ) {
          case text => ZIO.succeed(text)
        }
        res <- cliApp.run(List(command, "--opt", "inputText", "4"))

        // Check if the func checkAndGetOptionsFilePaths can

        _ <- cleanUpSampleConfigFiles2(cwd, command)

      } yield assertTrue(res == Some(("inputText", 4)))
    },
    test("failing test 3") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        command = "someCommandC"
        _       <- createSampleConfigFiles2(cwd, command)
        cliApp = CliApp.make(
          name = "cliApp",
          version = "0",
          summary = HelpDoc.Span.empty,
          command = Command(command, Options.text("opt"), Args.integer("num")),
        ) {
          case text => ZIO.succeed(text)
        }
        res <- cliApp.run(List(command, "4"))

        // Check if the func checkAndGetOptionsFilePaths can

        _ <- cleanUpSampleConfigFiles2(cwd, command)

      } yield assertTrue(res == Some(("inputText", 4)))
    },
    test("failing test 4") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        command = "someCommandD"
        _       <- createSampleConfigFiles2(cwd, command)
        cliApp = CliApp.make(
          name = "cliApp",
          version = "0",
          summary = HelpDoc.Span.empty,
          command = Command(command, Args.integer("num")),
        ) {
          case text => ZIO.succeed(text)
        }
        res <- cliApp.run(List(command, "4"))

        // Check if the func checkAndGetOptionsFilePaths can

        _ <- cleanUpSampleConfigFiles2(cwd, command)

      } yield assertTrue(res == Some(4))
    },

  )

  def createSampleConfigFiles(cwd: Path, homeDir: Path, command: String = "testApp"): IO[IOException, Unit] =
    ZIO.attempt {
      Files.write(Paths.get(homeDir.toString(), s".$command"), java.util.Arrays.asList("home=true"));
      Files.write(Paths.get(cwd.toString(), s".$command"), java.util.Arrays.asList("dir=true\nhome=false"));

      ()
    }.refineToOrDie[IOException]

  def cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command: String = "testApp"): IO[IOException, Unit] =
    ZIO.attempt {
      Files.delete(Paths.get(homeDir.toString(), s".$command"));
      Files.delete(Paths.get(cwd.toString(), s".$command"));

      ()
    }.refineToOrDie[IOException]


  def createSampleConfigFiles2(cwd: Path, command: String = "testApp"): IO[IOException, Unit] =
    ZIO.attempt {
      Files.write(Paths.get(cwd.toString(), s".$command"), java.util.Arrays.asList("--opt=fileText"));

      ()
    }.refineToOrDie[IOException]

  def cleanUpSampleConfigFiles2(cwd: Path, command: String = "testApp"): IO[IOException, Unit] =
    ZIO.attempt {
      Files.delete(Paths.get(cwd.toString(), s".$command"));

      ()
    }.refineToOrDie[IOException]
}
