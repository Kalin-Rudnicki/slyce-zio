package slyce.test.json

import harness.zio.*

import slyce.parse.exe.*

object Main extends ExecutableApp {
  override val executable: Executable = ParseExe.fromParser(JsonParser)("json")
}
