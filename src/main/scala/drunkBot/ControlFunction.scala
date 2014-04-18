package drunkBot

import scala.util.Random

/** This bot builds a 'direction value map' that assigns an attractiveness score to
  * each of the eight available 45-degree directions. Additional behaviors:
  * - aggressive missiles: approach an enemy master, then explode
  * - defensive missiles: approach an enemy slave and annihilate it
  *
  * The master bot uses the following state parameters:
  *  - dontFireAggressiveMissileUntil
  *  - dontFireDefensiveMissileUntil
  *  - lastDirection
  * The mini-bots use the following state parameters:
  *  - mood = Aggressive | Defensive | Lurking
  *  - target = remaining offset to target location
  */
object ControlFunction
{
  def rnd = new Random()
  def forMaster(bot: BotImpl) {
    // demo: log the view of the master bot into the debug output (if running in the browser sandbox)
    // bot.log(bot.view.cells.grouped(31).mkString("\n"))

    val (directionValue, nearestEnemyMaster, nearestEnemySlave) = analyzeViewAsMaster(bot.view)

    val dontFireAggressiveMissileUntil = bot.inputAsIntOrElse("dontFireAggressiveMissileUntil", -1)
    val dontFireDefensiveMissileUntil = bot.inputAsIntOrElse("dontFireDefensiveMissileUntil", -1)
    val lastDirection = bot.inputAsIntOrElse("lastDirection", 0)

    // determine movement direction
    directionValue(lastDirection) += 10 // try to break ties by favoring the last direction
    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val direction = XY.fromDirection45(bestDirection45)
    bot.move(direction)
    bot.set("lastDirection" -> bestDirection45)

    if(bot.view.spawnBot && bot.energy > 500 && scala.math.abs(bot.lastBotTime - bot.time) > 4){
      val direction = bot.view.emptyDirection
      direction match {
        case None =>
          bot.say("Let's PARTY!!!")
        case Some(pos) =>
          bot.spawn(pos, "mood" -> "harvest")
          bot.set("lastBotTime" -> bot.time)
      }
    }
  }


  def forSlave(bot: BotImpl) {
    if(reactAsAggressive(bot, 'm') || reactAsAggressive(bot, 's')){
      return
    }
    reactAsHarvest(bot)
  }


  def reactAsHarvest(bot: BotImpl) {
      val offsets = List(bot.view.offsetToNearest('P'), bot.view.offsetToNearest('B'))
      val part = offsets.filter(!_.isEmpty).map(_.get)
      val offsetToMaster = bot.offsetToMaster
      var offset = if(part.size == 0) offsetToMaster else part(0)

      if(!bot.view.isRelCellAv(offset.signum)){
        val directions = bot.view.allEmptyDirections
        val t = Random.shuffle(directions).find(p => p.x != bot.inputAsIntOrElse("rx", 0) && p.y != bot.inputAsIntOrElse("ry", 0))
        if(rnd.nextInt(10) >= 9)
          bot.say(if(rnd.nextInt(10) % 2 == 0) "I'm drunk" else "Shitfaced")
        t match {
          case Some(p) =>
            offset = p
          case None =>
            offset = XY(0,0)
        }
      }

      bot.move(offset.signum)
      bot.set("rx" -> offset.x, "ry" -> offset.y)


      if(bot.view.spawnBot && bot.energy > 400 && math.abs(bot.time - bot.lastBotTime) > 4) {
        val emptyCell = bot.view.emptyDirection
        emptyCell match {
          case Some(pos) =>
            bot.spawn(pos, "mood" -> "harvest")
            bot.set("lastBotTime" -> bot.time)
          case None =>
            bot.say("Let's PARTY!!!")
        }
      }
  }

  def reactAsAggressive(bot: MiniBot, t: Char): Boolean = {
    bot.view.offsetToNearest(t) match {
      case Some(delta: XY) =>
        // another creature is visible at the given relative position (i.e. position delta)
        if(delta.length <= 2) {
          // yes -- blow it up!
          bot.say("GERONIMO")
          bot.explode(4)
          return true
        } else {
          return false
//          // no -- move closer!
//          bot.move(delta.signum)
//          bot.set("rx" -> delta.x, "ry" -> delta.y)
        }
        return false
      case None =>
        return false
    }
    false
  }

  /** Analyze the view, building a map of attractiveness for the 45-degree directions and
    * recording other relevant data, such as the nearest elements of various kinds.
    */
  def analyzeViewAsMaster(view: View) = {
    val directionValue = Array.ofDim[Double](8)
    var nearestEnemyMaster: Option[XY] = None
    var nearestEnemySlave: Option[XY] = None

    val cells = view.cells
    val cellCount = cells.length
    for(i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if(cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case 'm' => // another master: not dangerous, but an obstacle
            nearestEnemyMaster = Some(cellRelPos)
            if(stepDistance < 2) -1000 else 0

          case 's' => // another slave: potentially dangerous?
            nearestEnemySlave = Some(cellRelPos)
            -100 / stepDistance

          case 'S' => // out own slave
            0.0

          case 'B' => // good beast: valuable, but runs away
            if(stepDistance == 1) 600
            else if(stepDistance == 2) 300
            else (150 - stepDistance * 15).max(10)

          case 'P' => // good plant: less valuable, but does not run
            if(stepDistance == 1) 500
            else if(stepDistance == 2) 300
            else (150 - stepDistance * 10).max(10)

          case 'b' => // bad beast: dangerous, but only if very close
            if(stepDistance < 4) -400 / stepDistance else -50 / stepDistance

          case 'p' => // bad plant: bad, but only if I step on it
            if(stepDistance < 2) -1000 else 0

          case 'W' => // wall: harmless, just don't walk into it
            if(stepDistance < 2) -1000 else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    (directionValue, nearestEnemyMaster, nearestEnemySlave)
  }
}
