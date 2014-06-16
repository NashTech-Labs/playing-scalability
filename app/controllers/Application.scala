package controllers

import java.util.concurrent.TimeoutException
import scala.concurrent.Future
import scala.concurrent.duration._
import models._
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.concurrent.Promise
import play.api.mvc._
import views._
import play.api.cache.Cached
import play.api.Play.current

object Application extends Controller {

  implicit val timeout = 10.seconds

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
  def list(page: Int, orderBy: Int, filter: String) = Action.async { implicit request =>
    val futurePage: Future[Page[Book]] = TimeoutFuture(Book.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")))
    futurePage.map(page => Ok(html.list(page, orderBy, filter))).recover {
      case t: TimeoutException =>
        Logger.error("Problem found in book list process")
        InternalServerError(t.getMessage)
    }
  }

  /**
   * Display the paginated list of books (cached + asynchronous + non-blocking).
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on book names
   */
  def asynchronousCached(page: Int, orderBy: Int, filter: String) = Cached("asynchronous") {
    Action.async { implicit request =>
      val futurePage: Future[Page[Book]] = TimeoutFuture(Book.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")))
      futurePage.map(page => Ok(html.list(page, orderBy, filter))).recover {
        case t: TimeoutException =>
          Logger.error("Problem found in book list process")
          InternalServerError(t.getMessage)
      }
    }
  }

  /**
   * Display the paginated list of books (cached + synchronous + blocking).
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on book names
   */
  def synchronousCached(page: Int, orderBy: Int, filter: String) = Cached("synchronous") {
    Action { implicit request =>
      val books = Book.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%"))
      Ok(html.list(books, orderBy, filter))
    }
  }

  /**
   * Display the paginated list of books (cached + synchronous + blocking).
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on book names
   */
  def synchronous(page: Int, orderBy: Int, filter: String) = Action { implicit request =>
    val books = Book.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%"))
    Ok(html.list(books, orderBy, filter))
  }

  /**
   * Display the 'edit form' of a existing book.
   *
   * @param id Id of the book to edit
   */
  def edit(id: Long) = Action.async {
    val futureEmp: Future[Option[Book]] = TimeoutFuture(Book.findById(id))
    futureEmp.map {
      case Some(book) => Ok(html.editForm(id, bookForm.fill(book)))
      case None       => NotFound
    }.recover {
      case t: TimeoutException =>
        Logger.error("Problem found in book edit process")
        InternalServerError(t.getMessage)
    }
  }

  /**
   * Handle the 'edit form' submission
   *
   * @param id Id of the book to edit
   */
  def update(id: Long) = Action.async { implicit request =>
    bookForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.editForm(id, formWithErrors))),
      book => {
        val futureUpdateEmp: Future[Int] = TimeoutFuture(Book.update(id, book))
        futureUpdateEmp.map { bookId =>
          Home.flashing("success" -> s"Book ${book.name} has been updated")
        }.recover {
          case t: TimeoutException =>
            Logger.error("Problem found in book update process")
            InternalServerError(t.getMessage)
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
  def save = Action.async { implicit request =>
    bookForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.createForm(formWithErrors))),
      book => {
        val futureUpdateEmp: Future[Option[Long]] = TimeoutFuture(Book.insert(book))
        futureUpdateEmp.map {
          case Some(bookId) =>
            val msg = s"Book ${book.name} has been created"
            Logger.info(msg)
            Home.flashing("success" -> msg)
          case None =>
            val msg = s"Book ${book.name} has not created"
            Logger.info(msg)
            Home.flashing("error" -> msg)
        }.recover {
          case t: TimeoutException =>
            Logger.error("Problem found in Book update process")
            InternalServerError(t.getMessage)
        }
      })
  }

  /**
   * Handle book deletion.
   */
  def delete(id: Long) = Action.async {
    val futureInt = TimeoutFuture(Book.delete(id))
    futureInt.map(i => Home.flashing("success" -> "Book has been deleted")).recover {
      case t: TimeoutException =>
        Logger.error("Problem deleting book")
        InternalServerError(t.getMessage)
    }
  }

  object TimeoutFuture {

    def apply[A](block: => A)(implicit timeout: FiniteDuration): Future[A] = {

      val promise = scala.concurrent.Promise[A]()

      // if the promise doesn't have a value yet then this completes the future with a failure
      Promise.timeout(Nil, timeout).map(_ => promise.tryFailure(new TimeoutException("This operation timed out")))

      // this tries to complete the future with the value from block
      Future(promise.success(block))

      promise.future
    }

  }

}