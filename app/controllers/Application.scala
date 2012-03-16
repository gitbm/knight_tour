package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Application extends Controller {
  
  val ID_KEY = "id"
  
  def doWithRedirect(f: => Unit) = { 
    f
	Redirect(routes.Application.knightTour)
  }
  
  def index = knightTourMain
 
  def knightTourMain = Action { Redirect(routes.Application.knightTour) }
  
  def knightTourRules = Action {  Ok(views.html.rules()) }
	
  def getId(implicit req: Request[AnyContent]) = req.session.get(ID_KEY).map(_.toInt ).filter(Board.validId(_)).getOrElse { Board.newId }
	    
  def knightTour = Action { implicit req =>
    val id = getId(req)
    Ok(views.html.board(id)).withSession(ID_KEY -> id.toString)
  }
	      
  def jump(row : Int, col : Int) = Action { implicit req => doWithRedirect { Board.jump(getId, row, col) } }

  def reset = Action { implicit req => doWithRedirect { Board.reset(getId) } }

  def undo =  Action { implicit req => doWithRedirect{ Board.undo(getId) } }

}