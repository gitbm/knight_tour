package models

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

case class CoOrds(row : Int, col : Int) {
  def abs(i : Int) = if (i < 0) -i else i
  def diff(other : CoOrds) = CoOrds(abs(row - other.row), abs(col - other.col))
}

class Board(rows : Int, cols: Int) {  
  var board : Array[Array[Option[Int]]] = _
 
  def reset { board = Array.fill(rows, rows)(Option.empty[Int]) }
  {reset}
  def apply(coOrds : CoOrds) = board(coOrds.row)(coOrds.col)
  def update(coOrds : CoOrds, num : Int) {  board(coOrds.row)(coOrds.col) = Some(num) }
  def clear(coOrds : CoOrds) { board(coOrds.row)(coOrds.col) = None }
}

object Board {
  
  val ROWS = 8
  val COLUMNS = 8

  var board : Board = new Board(ROWS, COLUMNS)
  var count : Int = _;
  var moves : List[CoOrds] = _;

  def init { count = 0; moves = Nil; }
  {init}
  def reset { init; board.reset }
  
  def isKnightMove(diff : CoOrds) = diff == CoOrds(1, 2) || diff == CoOrds(2, 1)
  
  def legalJump(newSquare: CoOrds) = moves.headOption.map(c => isKnightMove(c.diff(newSquare))).getOrElse(true)
  
  def jump(newSquare: CoOrds) {
    if (board(newSquare).isEmpty && legalJump(newSquare)) {
      count += 1
      moves ::= newSquare
      board(newSquare) = count
    }
  }
  
  def undo { 
    val head = moves.headOption
    if (head.isDefined) {
      count -= 1
      moves = moves.tail
      board.clear(head.get)
    }
  }
  
  def getSquareString(coOrds: CoOrds) = board(coOrds).map(_.toString).getOrElse("")

}