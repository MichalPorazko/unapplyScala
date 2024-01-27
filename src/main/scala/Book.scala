case class BookDetails(genre: String, pageCount: Int)

class Book(title: String, author: String, year: Int, bookDetails: BookDetails) {
  def loadBookDetails(): BookDetails =
    BookDetails("Fiction", 320)
}

object Book

  //zastanawiasm się nad taką rzcze no bo w przykladzie w ksiazce jest ze zwracamy (double, double) chat podaje z kolei
  //że zwracamy (String, Double), a co jeśli klasa ma wiele różnych typów zmiennych a my potrzbujemy różnie raz jednych typów raz innych
  // co wtedy??
  // np. w jednym patern matchingu potrzbujemy dwóch String i Int
  // a w innej Double i String o i co???
  def unapply(book: Book): Option[(String, Int)] =
    val bookDetails = book.loadBookDetails()
    Some((bookDetails.genre, bookDetails.pageCount))

sealed trait Category
case object Fiction extends Category
case object NonFiction extends Category
case object Short extends Category
case object Long extends Category

val fictionGenre = "Fiction"
val pageCountThreshold: Int = 300

//not using the unapply method
def analyse(book: Book): Set[Category] =
  book.loadBookDetails() match
    case Book(fiction, pageCount) if pageCount>= pageCountThreshold && fiction == fictionGenre => Set(Fiction, Long)
    case Book(_, pageCount) if pageCount>= pageCountThreshold => Set(Long)








