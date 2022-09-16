import scala.annotation.tailrec
import scala.io.StdIn.readLine

object PieceOrientation extends App{

  val pattern = """([0-7][0-7])?([0-7][0-7])"""


  def run(game: Game,turn:String):Unit={
    while(!game.winner){
      game.board.show()
      println(s"${turn} to play")
      val input = readLine()
      if(!(input matches pattern)) run(game,turn)
      val move = game.verify(new Command(input,"mouse"))

      turn match {
        case "mouse" => run(new Game(game.board.update(game.board.table((move.x,move.y)),(move.targetX,move.targetY))),"pawn")
        case "pawn" => run(new Game(game.board.update(game.board.table((move.x,move.y)),(move.targetX,move.targetY))),"mouse")
      }

      //run(new Game(game.board.update(game.board.table((move.x,move.y)),(move.targetX,move.targetY))),"pawn")
    }
  }

  run(new Game(new Board()),"mouse")

  abstract class Piece {
    val symbol:String
    val posVector :(Int,Int)

  }

  class Pawn(x:Int,y:Int) extends Piece{
    override val symbol: String = "P"
    override val posVector:(Int,Int)=(x,y)
  }
  class Mouse(x:Int,y:Int) extends Piece{
    override val symbol:String = "M"
    override val posVector:(Int,Int)=(x,y)
  }

  class Board(val table: Map[(Int,Int),Piece]=Map((0,0)->new Pawn(0,0),(2,0)-> new Pawn(2,0),(4,0)-> new Pawn(4,0),(6,0)-> new Pawn(6,0),(3,7)->new Mouse(3,7))){
    def initial():Board=new Board()

    def update(piece:Piece, destination:(Int,Int)):Board={
      piece.symbol match{
        case "P" => new Board(table-piece.posVector updated (destination,new Pawn(destination._1,destination._2)))
        case "M" => new Board(table-piece.posVector updated (destination,new Mouse(destination._1,destination._2)))
      }
    }
    def show():Unit ={
      @tailrec
      def rec(x:Int,y:Int):Unit={
        if(x==7&&y==7){
          if(table contains (7,7))println(table((7,7)).symbol) else println("-")
        }else{
          if(x==7){
            if(table contains (7,y)){
              println(table(7,y).symbol)
              rec(0,y+1)
            }else{
              println("-")
              rec(0,y+1)
            }
          }else{
            if(table contains (x,y)){
              print(table(x,y).symbol)
              rec(x+1,y)
            }else{
              print("-")
              rec(x+1,y)
            }
          }
        }
      }
      rec(0,0)
    }
  }
  class Game(val board:Board){
    val winner = false

    def verify(command:Command):Command={
      //first we check if the player intends to move the right piece
      if(command.turn=="mouse"){
        if(board.table(command.x,command.y).symbol=="M"){
          return command
        } else {
          println("tu n as pas choisi la bonne piece")
          new Command(readLine(),command.turn)
        }
      } else {
        if(board.table(command.x,command.y).symbol=="P") return command else new Command(readLine(),command.turn)
      }
      //now we check if the move is diagonal back front for mouse or diagonal forward for the pawn
      //mais il faut les combiner

    }
  }

  class Command(val move:String, val turn: String){

    val x = move(0).toString.toInt
    val y = move(1).toString.toInt
    val targetX = move(2).toString.toInt
    val targetY = move(3).toString.toInt

  }
}
