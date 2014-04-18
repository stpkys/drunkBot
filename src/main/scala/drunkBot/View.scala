package drunkBot

case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
  def absPosFromIndex(index: Int) = XY(index % size, index / size)
  def absPosFromRelPos(relPos: XY) = relPos + center
  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
  def relPosFromAbsPos(absPos: XY) = absPos - center
  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  def offsetToNearest(c: Char) = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if( matchingXY.isEmpty )
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      Some(nearest)
    }
  }

  def isRelCellAv(p: XY): Boolean = {
    val c = cells.charAt(indexFromRelPos(p))
    if(c == '_' || c == 'B' || c == 'P' || c == 'M') true else false
  }

  def allEmptyDirections: List[XY] = {
    var res = List[XY]()
    for(dx <- -1 to 1) {
      for(dy <- -1 to 1) {
        if(math.abs(dx) + math.abs(dy) > 1 && isRelCellAv(XY(dx, dy))) {
          res ::= XY(dx, dy)
        }
      }
    }
    res
  }

  def emptyDirection: Option[XY] = {
    val r = allEmptyDirections
    if(r.size == 0) None else Some(r(0))
  }

  def getFreq = cells.groupBy(_.toChar).map(p => (p._1, p._2.length))

  def spawnBot: Boolean = {
    val freq = getFreq
    if(freq.getOrElse('S', 0) > size*size / 2) return false
    if(freq.getOrElse('P', 0) + freq.getOrElse('B', 0) >= 2) return true
    if(freq.getOrElse('m', 0) + freq.getOrElse('s', 0) >= 2) return true
    false
  }
}