//1. Types de documents
// Trait Borrowable
trait Borrowable {
  def borrow(): Boolean
  def returnItem(): Boolean
}
//b. Créer une classe abstraite Document qui implémente Borrowable
// Classe abstraite Document: une classe non instanciable
  abstract class Document(
  val title: String,
  val author: String,
  val year: Int,
  var isBorrowed: Boolean = false // aucun document emprunté au départ
) extends Borrowable {

  // Méthodes de Borrowable
  override def borrow(): Boolean = {
    if (!isBorrowed) {
      true
    } else {
      false
    }
  }

  override def returnItem(): Boolean = {
    if (isBorrowed) {
      true
    } else {
      false
    }
  }

  // Méthode abstraite à implémenter dans les sous-classes
  def description(): String
}

//c. Créer trois classes héritant de Document : 
// Book
class Book(
  title: String,
  author: String,
  year: Int,
  val genre: String
) extends Document(title, author, year) {

  override def description(): String = {
    s"Book: '$title' by $author ($year), Genre: $genre, Borrowed: $isBorrowed"
  }
}

// Magazine
class Magazine(
  title: String,
  author: String,
  year: Int,
  val editionNumber: Int
) extends Document(title, author, year) {

  override def description(): String = {
    s"Magazine: '$title' by $author ($year), Edition No: $editionNumber, Borrowed: $isBorrowed"
  }
}

// Comic
class Comic(
  title: String,
  author: String,
  year: Int,
  val seriesVolume: Int
) extends Document(title, author, year) {

  override def description(): String = {
    s"Comic: '$title' by $author ($year), Volume: $seriesVolume, Borrowed: $isBorrowed"
  }
}

//2-GESTION DES UTILISATEURS
// Classe User
class User(val name: String) {

  private var borrowedDocs: List[Document] = List()

  // 1. Emprunter un document
  def borrowDocument(doc: Document): Boolean = {
    if (doc.borrow()) { // utilise la méthode borrow du document
      borrowedDocs = borrowedDocs :+ doc
      doc.isBorrowed = true // Marque le document comme emprunté
      true
    } else {
      false
    }
  }

  // 2. Retourner un document
  def returnDocument(doc: Document): Boolean = {
    if (borrowedDocs.contains(doc) && doc.returnItem()) {
      borrowedDocs = borrowedDocs.filterNot(_ == doc) // Retire le document de la liste des empruntés
      doc.isBorrowed = false // Marque le document comme retourné
      true
    } else {
      false
    }
  }

  // 3. Afficher la liste des documents empruntés
  def listBorrowedDocuments(): Boolean = {
    if (borrowedDocs.isEmpty) {
      println(s"$name n'a emprunté aucun document.")
      false
    } else {
      println(s"Documents empruntés par $name :")
      borrowedDocs.foreach(d => println(" - " + d.description()))
      true
    }
  }
}

//3-GESTION DE LA BIBLIOTHÈQUE
// Classe Library
class Library {

  private var documents: List[Document] = List()
  private var users: List[User] = List()

  // 1. Ajouter un document
  def addDocument(doc: Document): Unit = {
    documents = documents :+ doc
    println(s"Document ajouté : ${doc.description()}")
  }

  // 2. Enregistrer un utilisateur
  def addUser(user: User): Unit = {
    users = users :+ user
    println(s"Utilisateur ajouté : ${user.name}")
  }

  // 3. Lister les documents disponibles
  def listAvailableDocuments(): Unit = {
    val availableDocs = documents.filter(d => d.borrow())
    if (availableDocs.isEmpty) {
      println("Aucun document disponible.")
    } else {
      println("Documents disponibles :")
      availableDocs.foreach(d => println(" - " + d.description()))
    }
  }

  // Getters
  def getDocuments: List[Document] = documents
  def getUsers: List[User] = users
}

//4-programme principal
object Main extends App {
  // Création de la bibliothèque
  val library = new Library()

 // Ajouter des documents
  val doc1 = new Book("The Hobbit", "J.R.R. Tolkien", 1937, "Fantasy")
  val doc2 = new Magazine("National Geographic", "NatGeo", 2023, 150)
  val doc3 = new Comic("Spider-Man", "Stan Lee", 1963, 12)

  library.addDocument(doc1)
  library.addDocument(doc2)
  library.addDocument(doc3)

  // Ajouter des utilisateurs
  val user1 = new User("Alice")
  val user2 = new User("Bob")

  library.addUser(user1)
  library.addUser(user2)

  println("\n--- Documents disponibles au départ ---")
  library.listAvailableDocuments()

  println("\n--- Alice emprunte The Hobbit ---")
  user1.borrowDocument(doc1)
  user1.listBorrowedDocuments()
  library.listAvailableDocuments()

  println("\n--- Bob emprunte National Geographic ---")
  user2.borrowDocument(doc2)
  user2.listBorrowedDocuments()
  library.listAvailableDocuments()

  println("\n--- Alice retourne The Hobbit ---")
  user1.returnDocument(doc1)
  user1.listBorrowedDocuments()
  library.listAvailableDocuments()
}


