package models

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._


object Board {
  
  val ROWS = 8
  val COLUMNS = 8

  def newId = { boards :+= new Board(); boards.size - 1 }
  
  def validId(id : Int) = boards.isDefinedAt(id)
  
  def reset(id: Int) { boards(id).reset }
  
  def jump(id: Int, row: Int, col: Int) { boards(id).jump(CoOrds(row, col)) }
  
  def undo(id: Int) { boards(id).undo }
  
  def possibleMoves(id: Int) = boards(id).possibleMoves

  def totalMoves(id: Int) = boards(id).totalMoves

  def getSquareData(id: Int, row: Int, col: Int) = boards(id).getSquareData(CoOrds(row, col))
  
  // End of Public API
  
  // Private Implementation follows
  
  private abstract sealed class Status
  private case object Used extends Status
  private case object Current extends Status
  private case object OneMove extends Status
  private case object MultipleMoves extends Status
  private case object TwoFromEnd extends Status
  private case object OneFromEnd extends Status
  private case object Other extends Status
  
  private case class CoOrds(row: Int, col: Int) {
	  def abs(i: Int) = if (i < 0) -i else i
	  def diff(other: CoOrds) = CoOrds(abs(row - other.row), abs(col - other.col))
	  def add(other: CoOrds) = CoOrds(row + other.row, col + other.col)
	  def swap = CoOrds(col, row)
  }
	
  private case class BoardData(rows : Int, cols: Int) {  
	  private var board : Array[Array[Option[Int]]] = newBoard 
	  private def newBoard = Array.fill(rows, rows)(Option.empty[Int])
	  
	  def reset { board = newBoard }
	  def apply(coOrds : CoOrds) = board(coOrds.row)(coOrds.col)
	  def update(coOrds : CoOrds, num : Int) {  board(coOrds.row)(coOrds.col) = Some(num) }
	  def clear(coOrds : CoOrds) { board(coOrds.row)(coOrds.col) = None }
  }
	
  private val KNIGHT_MOVE_TEMPLATE = CoOrds(1, 2)
  private val BASIC_KNIGHT_MOVES = Seq(KNIGHT_MOVE_TEMPLATE, KNIGHT_MOVE_TEMPLATE.swap)
  private val MOVE_PERMUTATIONS = Seq((1, 1), (1, -1), (-1, 1), (-1, -1))

  private var boards = Vector[Board]()

}

private class Board {
  import Board._
  
  def jump(newSquare: CoOrds) {
    if (board(newSquare).isEmpty && legalJump(newSquare)) {
      moves ::= newSquare
      board(newSquare) = totalMoves
    }
  }

  def reset {  moves = Nil; board.reset }

  def undo { 
    moves = moves match {
      case Nil => moves
      case head::tail => { board.clear(head); tail }
    }
  }
  
  def totalMoves = moves.size

  def getSquareData(coOrds: CoOrds) = (getSquareStatus(coOrds).toString, getSquareString(coOrds))
  
  private var board = BoardData(ROWS, COLUMNS)
  private var moves : List[CoOrds] = Nil;

  private def getSquareString(coOrds: CoOrds) = board(coOrds).map(_.toString).getOrElse("")
  
  private def isKnightMove(start: CoOrds, finish: CoOrds) = BASIC_KNIGHT_MOVES.contains(start.diff(finish)) 
  
  private def legalJump(newSquare: CoOrds) = moves match {
      case Nil => true
      case head::_ => isKnightMove(head, newSquare)
  }
      
  private def inRange(num: Int, low: Int, high: Int) = num >= 0 && num < high
  
  private def legalCoOrds(coOrds : CoOrds) = inRange(coOrds.row, 0, ROWS) && inRange(coOrds.col, 0, COLUMNS) 
  
  private def possibleMoves(coOrds: CoOrds) = 
    (for (p <- MOVE_PERMUTATIONS; k <- BASIC_KNIGHT_MOVES) 
         yield CoOrds(k.row * p._1, k.col * p._2))
    		 .map(c => coOrds.add(c)).filter(c => legalCoOrds(c) && board(c).isEmpty)
  
  private def possibleMoves: Int = moves match {
      case Nil => 64
      case head::_ => possibleMoves(head).size
  }
  
  private def getSquareStatus(coOrds: CoOrds) = moves match {
    case head::_ if  head == coOrds => Current
    case head::_ if possibleMoves(head).contains(coOrds) => 
      possibleMoves(coOrds).size match  {
        case 0 => OneFromEnd 
        case _ if ( (possibleMoves(coOrds).filter(c => possibleMoves(c).size > 1).size) == 0) => TwoFromEnd
        case 1 => OneMove
        case _ => MultipleMoves
      }
    case _ => board(coOrds).map(_ => Used).getOrElse(Other)
  }
}

