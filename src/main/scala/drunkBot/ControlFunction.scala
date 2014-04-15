package drunkBot

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
  def forMaster(bot: Bot) {
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

    if(dontFireAggressiveMissileUntil < bot.time && bot.energy > 100) { // fire attack missile?
      nearestEnemyMaster match {
        case None =>            // no-on nearby
        case Some(relPos) =>    // a master is nearby
          val unitDelta = relPos.signum
          val remainder = relPos - unitDelta // we place slave nearer target, so subtract that from overall delta
          bot.spawn(unitDelta, "mood" -> "Aggressive", "target" -> remainder)
          bot.set("dontFireAggressiveMissileUntil" -> (bot.time + relPos.stepCount + 1))
      }
    }
    else
    if(dontFireDefensiveMissileUntil < bot.time && bot.energy > 100) { // fire defensive missile?
      nearestEnemySlave match {
        case None =>            // no-on nearby
        case Some(relPos) =>    // an enemy slave is nearby
          if(relPos.stepCount < 8) {
            // this one's getting too close!
            val unitDelta = relPos.signum
            val remainder = relPos - unitDelta // we place slave nearer target, so subtract that from overall delta
            bot.spawn(unitDelta, "mood" -> "Defensive", "target" -> remainder)
            bot.set("dontFireDefensiveMissileUntil" -> (bot.time + relPos.stepCount + 1))
          }
      }
    }
  }


  def forSlave(bot: MiniBot) {
    bot.inputOrElse("mood", "Lurking") match {
      case "Aggressive" => reactAsAggressiveMissile(bot)
      case "Defensive" => reactAsDefensiveMissile(bot)
      case s: String => bot.log("unknown mood: " + s)
    }
  }


  def reactAsAggressiveMissile(bot: MiniBot) {
    bot.view.offsetToNearest('m') match {
      case Some(delta: XY) =>
        // another master is visible at the given relative position (i.e. position delta)

        // close enough to blow it up?
        if(delta.length <= 2) {
          // yes -- blow it up!
          bot.explode(4)

        } else {
          // no -- move closer!
          bot.move(delta.signum)
          bot.set("rx" -> delta.x, "ry" -> delta.y)
        }
      case None =>
        // no target visible -- follow our targeting strategy
        val target = bot.inputAsXYOrElse("target", XY.Zero)

        // did we arrive at the target?
        if(target.isNonZero) {
          // no -- keep going
          val unitDelta = target.signum // e.g. CellPos(-8,6) => CellPos(-1,1)
          bot.move(unitDelta)

          // compute the remaining delta and encode it into a new 'target' property
          val remainder = target - unitDelta // e.g. = CellPos(-7,5)
          bot.set("target" -> remainder)
        } else {
          // yes -- but we did not detonate yet, and are not pursuing anything?!? => switch purpose
          bot.set("mood" -> "Lurking", "target" -> "")
          bot.say("Lurking")
        }
    }
  }


  def reactAsDefensiveMissile(bot: MiniBot) {
    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // another slave is visible at the given relative position (i.e. position delta)
        // move closer!
        bot.move(delta.signum)
        bot.set("rx" -> delta.x, "ry" -> delta.y)

      case None =>
        // no target visible -- follow our targeting strategy
        val target = bot.inputAsXYOrElse("target", XY.Zero)

        // did we arrive at the target?
        if(target.isNonZero) {
          // no -- keep going
          val unitDelta = target.signum // e.g. CellPos(-8,6) => CellPos(-1,1)
          bot.move(unitDelta)

          // compute the remaining delta and encode it into a new 'target' property
          val remainder = target - unitDelta // e.g. = CellPos(-7,5)
          bot.set("target" -> remainder)
        } else {
          // yes -- but we did not annihilate yet, and are not pursuing anything?!? => switch purpose
          bot.set("mood" -> "Lurking", "target" -> "")
          bot.say("Lurking")
        }
    }
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
