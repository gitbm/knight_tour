package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Application extends Controller {
  
  val taskForm = Form("label" -> nonEmptyText)
	
  def index = Action {
    //Ok(views.html.index("Your new application is ready."))
    Redirect(routes.Application.tasks)
  }
 
  def tasks = Action {
	Ok(views.html.i2(Task.all(), taskForm )) 
  }
	
  def buttonTask(r : Int, c : Int) = Action {
	  Board.jump(r, c)
	  Redirect (routes.Application.tasks)
  }
  
  def newTask = Action { implicit request =>
  	taskForm.bindFromRequest .fold(
  		errors => BadRequest (views.html.index(Task.all(), errors)),
		label => {
			Task.create(label)
			Redirect (routes.Application.tasks)
		}
	  )
  }

  def resetTask = Action {
	  Board.reset
	  Redirect (routes.Application.tasks)
  }
  
  def deleteTask (id: Long) = Action {
	  Task.delete(id)
	  Redirect (routes.Application.tasks)
  }


}