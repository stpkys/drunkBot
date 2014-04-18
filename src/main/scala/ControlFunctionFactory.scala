import drunkBot._
import drunkBot.BotImpl
import collection.JavaConversions._

class ControlFunctionFactory {
  var apocalypse: Int = -1
  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" =>
        try {
          val bot = new BotImpl(params)
          val dbot = new DBot()
          dbot.init(mapAsJavaMap(params))
          if (bot.generation == 0) {
            ControlFunction.forMaster(bot)
          } else {
            ControlFunction.forSlave(bot)
          }
          bot.toString
        } catch {
          case e: Exception =>
            println("Exception" + e)
            println(e.printStackTrace())
            "Say(text=Ooops)"
        }
      case "Welcome" =>
        apocalypse = params("apocalypse").toInt
        ""
      case "Goodbye" => ""
      case _ => "" // OK
    }
  }
}
