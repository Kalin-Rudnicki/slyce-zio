package slyce.test.json

import oxygen.predef.executable.*

import slyce.parse.exe.*

object Main extends ExecutableApp {
  override val executable: Executable = ParseExe.fromParser(JsonParser)("json")
}
