package controllers

import models._
import play.api._
import play.api.Play.current
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import views._
import play.api.cache.Cached

object Application extends Controller {
  
  /**
   * Describe the book form (used in both edit and create screens).
   */
  val bookForm = Form(
    mapping(
      "id" -> ignored(0: Long),
      "name" -> nonEmptyText,
      "author" -> nonEmptyText,
      "publishDate" -> date("yyyy-MM-dd"),
      "description" -> nonEmptyText)(Book.apply)(Book.unapply))

  /**
   * Handle default path requests, redirect to book list
   */
  def index = Action { Home }

  /**
   * This result directly redirect to the application home.
   */
  val Home = Redirect(routes.Application.list())

  /**
   * Display the paginated list of books (asynchronous + non-blocking).
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on book names
   */
  def list(page: Int, orderBy: Int, filter: String) = Action { implicit request =>
    val pageData: Page[Book] = Book.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%"))
    Ok(html.list(pageData, orderBy, filter))
  }

  /**
   * Display the paginated cachedList of books (cached + synchronous + blocking).
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on book names
   */
  def cachedList(page: Int, orderBy: Int, filter: String) = Cached("cached").default(5) {
    Action { implicit request =>
      val books = Book.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%"))
      Ok(html.list(books, orderBy, filter))
    }
  }

  /**
   * Display the 'edit form' of a existing book.
   *
   * @param id Id of the book to edit
   */
  def edit(id: Long) = Action {
    Book.findById(id) match {
      case Some(book) => Ok(html.editForm(id, bookForm.fill(book)))
      case None       => NotFound
    }
  }

  /**
   * Handle the 'edit form' submission
   *
   * @param id Id of the book to edit
   */
  def update(id: Long) = Action { implicit request =>
    bookForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.editForm(id, formWithErrors)),
      book => {
        Book.update(id, book) > 0 match {
          case true  => Home.flashing("success" -> s"Book ${book.name} has been updated")
          case false => Home.flashing("error" -> "Problem found in book update process")
        }
      })
  }

  /**
   * Display the 'new book form'.
   */
  def create = Action {
    Ok(html.createForm(bookForm))
  }

  /**
   * Handle the 'new employee form' submission.
   */
  def save = Action { implicit request =>
    bookForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.createForm(formWithErrors)),
      book => {
        Book.insert(book) match {
          case Some(bookId) =>
            val msg = s"Book ${book.name} has been created"
            Home.flashing("success" -> msg)
          case None =>
            val msg = s"Book ${book.name} has not created"
            Home.flashing("error" -> msg)
        }
      })
  }

  /**
   * Handle book deletion.
   */
  def delete(id: Long) = Action {
    Book.delete(id) > 0 match {
      case true  => Home.flashing("success" -> "Book has been deleted")
      case false => Home.flashing("error" -> "Book not found for the given id.")
    }

  }

}