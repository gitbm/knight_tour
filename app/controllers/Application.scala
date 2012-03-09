package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Application extends Controller {
  
  def index = Action {
    Redirect(routes.Application.knightTour)
  }
 
  def knightTour = Action {
	Ok(views.html.index())
  }
	
  def jump(r : Int, c : Int) = Action {
	  Board.jump(CoOrds(r, c))
	  Redirect (routes.Application.knightTour)
  }
  
  def reset = Action {
	  Board.reset
	  Redirect (routes.Application.knightTour)
  }
}