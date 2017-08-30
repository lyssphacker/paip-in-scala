package paip.chapter18

import paip.chapter18.Othello.Piece.Piece
import paip.chapter18.Othello._

import scala.collection.mutable.ArrayBuffer

object Othello2 {
  val allSquares: List[Int] = (11 to 88).filter((i: Int) => {
    val mod = i % 10
    mod >= 1 && mod <= 8
  }).toList.sortWith(weights(_) > weights(_))

  case class Node(square: Option[Int] = None, board: Board, var value: Int)

  def alphaBetaSearcher2(depth: Int, evalFn: (Piece, Board) => Int): (Piece, Board) => Int = {
    (player: Piece, board: Board) => {
      val result = alphaBeta2(player,
        Node(board = board, value = evalFn.apply(player, board)),
        Board.LosingValue, Board.WinningValue, depth, evalFn)
      result._2.get.square.get
    }
  }

  def alphaBeta2(player: Piece,
                 node: Node,
                 achievable: Int,
                 cutoff: Int,
                 ply: Int,
                 evalFn: (Piece, Board) => Int): (Int, Option[Node]) = {
    if (ply == 0) (node.value, Some(node))
    else {
      val board = node.board
      val nodes = legaNodes(player, board, evalFn)
      if (nodes.isEmpty) {
        if (board.anyLegalMove(opponent(player))) {
          (-alphaBeta2(opponent(player), negateValue(node),
            -cutoff, -achievable, -ply, evalFn)._1,
            None)
        } else {
          (board.finalValue(player), None)
        }
      } else {
        var bestNode = nodes.head
        var achievable_ = achievable
        nodes.iterator.takeWhile((move: Node) => achievable_ >= cutoff).
          foreach((move: Node) => {
            val value = -alphaBeta2(opponent(player), negateValue(move),
              -cutoff, -achievable_, ply - 1, evalFn)._1
            if (value > achievable_) {
              achievable_ = value
              bestNode = move
            }
          })
        (achievable_, Some(bestNode))
      }
    }
  }

  def alphaBeta3(player: Piece,
                 board: Board,
                 achievable: Int,
                 cutoff: Int,
                 ply: Int,
                 evalFn: (Piece, Board) => Int,
                 killer: Option[Int]): (Int, Option[Int]) = {
    if (ply == 0) (evalFn.apply(player, board), None)
    else {
      val moves = putFirst(killer, legalMoves(player, board))
      if (moves.isEmpty) {
        if (board.anyLegalMove(player))
          (-alphaBeta3(opponent(player), board, -cutoff, -achievable, ply - 1, evalFn, None)._1, None)
        else (board.finalValue(player), None)
      } else {
        var bestMove = moves.head
        val newBoard = PlyBoards(ply)
        var killer2: Option[Int] = None
        var killer2Val = Board.WinningValue
        var achievable_ = achievable
        moves.iterator.takeWhile((i: Int) => achievable_ >= cutoff).
          foreach((move: Int) => {
            val result = alphaBeta3(opponent(player),
              Board(replace(newBoard.pieces.to[ArrayBuffer], board.pieces)).makeMove(move, player),
              -cutoff, -achievable_, ply - 1, evalFn, killer2)
            result match {
              case (value, reply) => {
                if (-value > achievable_) {
                  achievable_ = value
                  bestMove = value
                }
                if (reply.isDefined && value < killer2Val) {
                  killer2 = reply
                  killer2Val = value
                }
              }
            }
          })
        (achievable_, Some(bestMove))
      }
    }
  }

  def replace(ab: ArrayBuffer[Piece], arr: Array[Piece]): Array[Piece] = {
    for (i <- ab.indices) ab(i) = arr(i)
    arr
  }

  def putFirst(killer: Option[Int], moves: List[Int]): List[Int] = {
    if (killer.isDefined && moves.contains(killer.get))
      killer.get :: moves.filter((m: Int) => !m.equals(killer.get))
    else moves
  }

  def legaNodes(player: Piece, board: Board, evalFn: (Piece, Board) => Int): List[Node] = {
    val moves = legalMoves(player, board)
    moves.map((move: Int) => {
      val newBoard = board.copy().makeMove(move, player)
      Node(Some(move), newBoard, evalFn.apply(player, newBoard))
    }).sortWith(_.value > _.value)
  }

  def negateValue(node: Node): Node = {
    node.value = -node.value
    node
  }

  val PlyBoards = Array.fill[Board](40)(initialBoard())
}
