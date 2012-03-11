package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Application extends Controller {
  
  def doWithRedirect(id: Int)(f: => Unit) = Action {
    f
	Redirect(routes.Application.knightTour(id))
  }
  
  def index = knightTourMain//Action { Redirect(routes.Application.knightTourMain) }
  //def index = Action { Ok(views.html.index()) }
 
  def knightTourMain = Action { Redirect(routes.Application.knightTour(Board.newId)) }
  
  def knightTourRules = Action {  Ok(views.html.rules()) }
	
  def knightTour(id: Int) = Action { Ok(views.html.board(id)) }
	
  def jump(id: Int, r : Int, c : Int) = doWithRedirect(id) { Board.jump(id, CoOrds(r, c)) }
  
  def reset(id: Int) = doWithRedirect(id) { Board.reset(id) }

  def undo(id: Int) =  doWithRedirect(id) { Board.undo(id) }
}