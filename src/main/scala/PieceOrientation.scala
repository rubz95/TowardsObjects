import HelloSBT.stage

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scalafx.scene.paint.Color._
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.layout.BorderPane
import scalafx.scene.shape.Rectangle


object PieceOrientation extends App{
  /*
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      scene = new Scene {
        fill = Black
        content = new Rectangle {
          x = 50
          y = 75
          width = 25
          height = 25
          fill = Green
        }
      }
    }
  }

   */
  val pattern = """([0-7][0-7])?([0-7][0-7])"""


  def run(game: Game,turn:String):Unit={
    while(!game.winner){
      game.board.show()
      println(s"${turn} to play")
      val input = readLine()
      if(!(input matches pattern)) run(game,turn)
      val move = game.verify(new Command(input,turn))

      turn match {
        case "mouse" => run(new Game(game.board.update(game.board.table((move.x,move.y)),(move.targetX,move.targetY))),"pawn")
        case "pawn" => run(new Game(game.board.update(game.board.table((move.x,move.y)),(move.targetX,move.targetY))),"mouse")
      }
    }
    println("the game is over")
  }

  run(new Game(new Board()),"mouse")

  abstract class Piece {
    val symbol:String
    val posVector :(Int,Int)
    val movements: List[(Int,Int)]
  }

  class Pawn(x:Int,y:Int) extends Piece{
    override val symbol: String = "P"
    override val posVector:(Int,Int)=(x,y)
    override val movements: List[(Int, Int)] = List((1,-1),(1,1))
  }
  class Mouse(x:Int,y:Int) extends Piece{
    override val symbol:String = "M"
    override val posVector:(Int,Int)=(x,y)
    override val movements: List[(Int, Int)] = List((1,-1),(1,1),(-1,-1),(-1,1))
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
      for( i <- 0 until 8 ){println()
        for(j <-0 until 8){if(table contains(j,i)) print(table(j,i).symbol) else print("-")}

      }
      println()
    }
  }
  class Game(val board:Board){

    val game = Game
    val winner = game.mouseWon||game.pawnsWon

    def verify(command:Command):Command={
      //first we check if the player intends to move the right piece
      if(command.turn=="mouse"){
        if((command.x,command.y)==game.mousePos){
          if(game.mouseAvailableSquares contains (command.targetX,command.targetY)){
            println("tu as le droit de bouger la")
            command
          } else {
            println("selectionne la souris")
            verify(new Command(readLine(),command.turn))
          }
        } else {
          println("tu n as pas choisi la bonne piece")
          verify(new Command(readLine(),command.turn))
        }
      } else {

        if(game.pawnPositions contains (command.x,command.y)){
          if(vectorAdd((command.x,command.y),(1,-1))==(command.targetX,command.targetY)||vectorAdd((command.x,command.y),(1,1))==(command.targetX,command.targetY)){
            command
          } else {
            println("tu ne peux pas aller la")
            verify(new Command(readLine(),command.turn))
          }

        }  else {
          println("selectionne un pion")
          verify(new Command(readLine(),command.turn))
        }
      }
      //now we check if the move is diagonal back front for mouse or diagonal forward for the pawn
      //mais il faut les combiner

    }

    def getPieceAvailableMovements(piece: Piece):List[(Int,Int)]= rec(piece.movements,piece)

    def rec(l:List[(Int,Int)],piece: Piece):List[(Int,Int)]={
      if(l.isEmpty) Nil else {
        if(vectorAdd(l.head,piece.posVector)._1 <0||vectorAdd(l.head,piece.posVector)._1 >7||vectorAdd(l.head,piece.posVector)._2 <0 ||vectorAdd(l.head,piece.posVector)._2 >7){
          rec(l.tail,piece)
        }else if(board.table isDefinedAt vectorAdd(l.head,piece.posVector)){
          rec(l.tail,piece)
        }else{
          l.head::rec(l.tail,piece)
        }
      }
    }

    def vectorAdd(v1:(Int,Int),v2:(Int,Int)):(Int,Int)=(v1._1+v2._1,v1._2+v2._2)

    def canMouseMove:Boolean= if (game.mouseAvailableMoves==Nil) false else true



    object Game {
      val mousePos: (Int, Int) = board.table.filter(t=>t._2.symbol=="M").keys.head
      val pawnPositions: List[(Int, Int)] = board.table.filter(t=>t._2.symbol=="P").keys.toList
      val mouseAvailableMoves: List[(Int,Int)]= getPieceAvailableMovements(board.table(mousePos))

      val mouseAvailableSquares: List[(Int, Int)] = mouseAvailableMoves.map(vectorAdd(mousePos,_))

      val pawnsAvailableSquares: List[(Int,Int)] = pawnsAllSquares(pawnPositions)

      println(s"mouse ${mousePos} available squares ${mouseAvailableSquares}")
      println(s"pawns ${pawnPositions} available squares ${pawnsAvailableSquares}")

      val mouseReachedLast: Boolean = if(mousePos._2==0) true else false
      val mousePassedPawns: Boolean = mousePassed(pawnPositions)

      val mouseWon: Boolean = mouseReachedLast||mousePassedPawns
      val pawnsWon: Boolean = if(mouseAvailableMoves==Nil) true else false

      if(mouseWon)println(s"mouse won")
      if(pawnsWon)println(s"pawns won")

      @tailrec
      def mousePassed(l:List[(Int,Int)]):Boolean= {
        l match {
          case Nil => true
          case _ => if(l.head._2<=mousePos._2) false else mousePassed(l.tail)
        }
      }

      def pawnsAllSquares(l:List[(Int,Int)]):List[(Int,Int)] ={
        l match {
          case Nil => Nil
          case _ => getPieceAvailableMovements(board.table(l.head)).map(vectorAdd(l.head,_))++pawnsAllSquares(l.tail)
        }
      }

    }

  }

  class Command(val move:String, val turn: String){

    val x = move(0).toString.toInt
    val y = move(1).toString.toInt
    val targetX = move(2).toString.toInt
    val targetY = move(3).toString.toInt

  }

}
