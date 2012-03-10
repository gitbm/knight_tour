package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Application extends Controller {
  
  def doWithRedirect(f: => Unit) = Action {
    f
	Redirect(routes.Application.knightTour)
  }
  
  def index =  doWithRedirect { }
 
  def knightTour = Action { Ok(views.html.index()) }
	
  def jump(r : Int, c : Int) = doWithRedirect { Board.jump(CoOrds(r, c)) }
  
  def reset = doWithRedirect { Board.reset }

  def undo =  doWithRedirect { Board.undo }
}