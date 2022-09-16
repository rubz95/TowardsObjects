object ObjectOrientation extends App{

  class Animal{
    val age: Int = 0
    def eat()= println("I am eating")
  }

  val anAnimal = new Animal

  class Dog(val name:String) extends Animal

  val aDog = new Dog("LOLO")
  println(aDog.name)

  val aDeclaredAnimal: Animal = new Dog("Hachi")

  aDeclaredAnimal.eat() //the most derived method will be called at runtime

  //abstract class

  abstract class WalkingAnimal{
    val hasLegs = true //by default public, can be restricted by using private or protected
    def walk(): Unit //will need to be overrided
  }
  //Interface
  trait Carnivore {
    def eat(animal:Animal):Unit
  }

  trait Philosopher{
    def ?!(thought:String):Unit //valid method name
  }

  class Crocodile extends Animal with Carnivore with Philosopher{
    override def eat(animal:Animal):Unit = println("I am eating another animal")
    override def ?!(thought:String):Unit = println(s"I was thinking ${thought}")
  }


  val aCroc = new Crocodile()
  aCroc.eat(aDog)
  aCroc eat aDog //because ONE argument, infix notation, same as above
  aCroc ?! "what if we could fly?"
  //operators in scala are actually methods

  //anonymous classes

  val dinosaur = new Carnivore{
    override def eat(animal: Animal):Unit = println("I am a dino so I can eat pretty much anything")
  }

  //singleton object

  object mySingleTon{
    val mySpecValue = 32
    def mySpecMethod():Int= 432
    def apply(x:Int): Int = x+1
  } //only instance of the mysingleton

  mySingleTon.mySpecValue
  mySingleTon.apply(65)
  mySingleTon(65) //equivalent as the previous line

  object Animal { //companions
    //companions can access thers private fields/methods
    //singleton Animal and Sintance of Animal are different things
    val canLiveIndefenitely = false
  }

  val animalsCanLiveForever = Animal.canLiveIndefenitely //static fields or methods

  //case classes are loightweight data structures
  // sensible equals and hash code
  //serialization
  //companion with apply

  case class Person(name:String, age:Int)
  //no need new
  val bob = Person("Bob",54) //Person.apply("Bob",54)

  //exceptions
  try {
    //code that can throw
    val x:String = null
    x.length
  } catch {
    case e: Exception => "Some faulty error message"
  } finally {
    //execute some code no matter what
  }

  //generics

  abstract class MyList[T]{
    def head: T
    def tail: MyList[T]
  }
  //using a generic with a concrete type
  val aList:List[Int] = List(1,2,3) //List.apply(1,2,3)
  val first = aList.head
  val rest = aList.tail
  val aStringList = List("hello","scala")
  val firstString = aStringList.head //String

  val reversedList = aList.reverse
}
