package paip.chapter18

import paip.chapter18.Othello.Piece.Piece
import paip.chapter18.Othello.{Board, weights, legalMoves, opponent}

object Othello2 {
  val allSquares: List[Int] = (11 to 88).filter((i: Int) => {
    val mod = i % 10
    mod >= 1 && mod <= 8
  }).toList.sortWith(weights(_) > weights(_))

  case class Node(square: Int, board: Board, var value: Int)

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
        val bestNode = nodes.head
        for (move <- nodes) {
          val value = 
        }
      }

    }
  }

  def legaNodes(player: Piece, board: Board, evalFn: (Piece, Board) => Int): List[Node] = {
    val moves = legalMoves(player, board)
    moves.map((move: Int) => {
      val newBoard = board.copy().makeMove(move, player)
      Node(move, newBoard, evalFn.apply(player, newBoard))
    }).sortWith(_.value > _.value)
  }

  def negateValue(node: Node): Node = {
    node.value = -node.value
    node
  }
}
