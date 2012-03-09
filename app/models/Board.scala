package models

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

case class Square(used : Boolean, num : Int)

object Board {
  
  { reset }
  
  var board : Array[Array[Square]] = _
  
  var count : Int = _;
  var currSquare : Pair[Int, Int] = _;
  
  def makeCleanBoard = { Array.fill(8, 8)(Square(false, 0)) }
  
  def reset { count = 0; currSquare = (-1, -1); board = makeCleanBoard }
  
  def get(sq : (Int, Int)) = board(sq._1)(sq._2)
  
  def set(sqPr : (Int, Int), sq : Square) = { board(sqPr._1)(sqPr._2) = sq }
  
  def abs(i : Int) = if (i < 0) -i else i
  
  def legalJump(newSquare : (Int, Int)) = {
    currSquare._1 < 0 || 
    (abs(newSquare._1 - currSquare._1) == 2 && abs(newSquare._2 - currSquare._2) == 1) ||
    (abs(newSquare._1 - currSquare._1) == 1 && abs(newSquare._2 - currSquare._2) == 2)
  }
  
  def jump(newSquare : (Int, Int)) {
    if (!get(newSquare).used && legalJump(newSquare)) {
      count += 1
      currSquare = newSquare
      set(newSquare, Square(true, count))
    }
  }
  
  def getSquareString(r : Int, c : Int) = { val sq = get(r, c); if (sq.used) sq.num.toString else "    " }

}