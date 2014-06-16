package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps
import play.api.Logger

case class Book(id: Long, name: String, author: String, publishDate: Date, description: String)

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

object Book {

  // -- Parsers

  /**
   * Parse a Book from a ResultSet
   */
  val book = {
    get[Long]("book.id") ~
      get[String]("book.name") ~
      get[String]("book.author") ~
      get[Date]("book.publish_date") ~
      get[String]("book.description") map {
        case id ~ name ~ author ~ publishDate ~ description => Book(id, name, author, publishDate, description)
      }
  }

  // -- Queries

  /**
   * Retrieve a book from the id.
   */
  def findById(id: Long): Option[Book] = {
    DB.withConnection { implicit connection =>
      SQL("select * from book where id = {id}").on('id -> id).as(book.singleOpt)
    }
  }

  /**
   * Return a page of (Book).
   *
   * @param page Page to display
   * @param pageSize Number of books per page
   * @param orderBy Book property used for sorting
   * @param filter Filter applied on the name column
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%"): Page[Book] = {

    val offest = pageSize * page

    DB.withConnection { implicit connection =>

      val books = SQL(
        """
          select * from book 
          where book.name like {filter}
          order by {orderBy} nulls last
          limit {pageSize} offset {offset}
        """).on(
          'pageSize -> pageSize,
          'offset -> offest,
          'filter -> filter,
          'orderBy -> orderBy).as(book *)

      val totalRows = SQL(
        """
          select count(*) from book 
          where book.name like {filter}
        """).on(
          'filter -> filter).as(scalar[Long].single)

      Page(books, page, offest, totalRows)

    }

  }

  /**
   * Retrieve all Book.
   *
   * @return
   */
  def findAll(): List[Book] = {
    DB.withConnection { implicit connection =>
      try {
        SQL("select * from book order by name").as(book *)
      }
      catch {
        case ex: Exception => Logger.info("ERROR", ex); Nil
      }
    }
  }

  /**
   * Update a book.
   *
   * @param id The book id
   * @param employee The book values.
   */
  def update(id: Long, book: Book): Int = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update book
          set name = {name}, author = {author}, publish_date = {publishDate}, description = {description}
          where id = {id}
        """).on(
          'id -> id,
          'name -> book.name,
          'author -> book.author,
          'publishDate -> book.publishDate,
          'description -> book.description).executeUpdate()
    }
  }

  /**
   * Insert a new book.
   *
   * @param book The book values.
   */
  def insert(book: Book): Option[Long] = {
    DB.withConnection { implicit connection =>
      SQL("""insert into book values ({name}, {author}, {publishDate}, {description})""").on(
        'name -> book.name,
        'author -> book.author,
        'publishDate -> book.publishDate,
        'description -> book.description).executeInsert()
    }
  }

  /**
   * Delete a book.
   *
   * @param id Id of the book to delete.
   */
  def delete(id: Long): Int = {
    DB.withConnection { implicit connection =>
      SQL("delete from book where id = {id}").on('id -> id).executeUpdate()
    }
  }

}
