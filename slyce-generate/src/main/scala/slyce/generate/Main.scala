package slyce.generate

import klib.utils.*
import klib.utils.commandLine.parse.*
import zio.*

object Main extends ExecutableApp {

  private object generate {

    private val single: Executable =
      Executable
        .fromParser(Parser.unit.disallowExtras)
        .withLayer(_ => ZIO.unit.toLayer)
        .withExecute { _ =>
          Logger.println.info("Running generate/single")
        }

    val executable: Executable =
      Executable.fromSubCommands(
        "single" -> single,
      )

  }

  override val executable: Executable =
    Executable.fromSubCommands(
      "generate" -> generate.executable,
    )

}
