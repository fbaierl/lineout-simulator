package com.github.fbaierl.lineout

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Main extends IOApp {

  class InvalidInputError(msg: String)

  case class Player (name: String)
  case class Team (players: List[Player])
  case class LineoutState ()

  def printWelcomeMessage: IO[Unit] = IO(println(
    """
      |--------------------------------------
      |------ Rugby Lineout Simulator -------
      |--------------------------------------
      |-- First add 7 players to your team --
        """
    .stripMargin))

  def printPlayerInputInstructions: IO[Unit] = IO(println(
    """
      |Input format:
      |<name> [jump] [lift]
      |The name of a player is mandatory and 'jump' and 'lift'
      |are optional and describe the players capabilities."
    """.stripMargin
  ))

  def run(args: List[String]): IO[ExitCode] = for {
      _     <- printWelcomeMessage
      team  <- readTeam
      _     <- IO { println("Here is your team: " + team) }
    } yield ExitCode.Success

  /**
    * Reads a team of 7 players from the command line.
    * @return
    */
  def readTeam: IO[Team] = {
    def go(team: Team): IO[Team] = {
      if(team.players.size >= 7){
        IO(println("Enough players!!")) >>= (_ => IO(team))
      } else {
        for {
          player <- readPlayer
          team   <- go(Team(team.players :+ player))
        } yield team
      }
    }
    go(Team(Nil))
  }

  /**
    * Reads a player from the command-line
    * @return the player
    */
  private def readPlayer: IO[Player] = {
    def go(p: Option[Player]): IO[Option[Player]] = {
      if(p.isDefined) IO(p)
      else for {
        playerOpt <- readPlayerOpt
        player    <- go(playerOpt)
      } yield player
    }
    go(None) map (_.get)
  }

  /**
    * Reads a player from the command-line
    * @return the player or None
    */
  private def readPlayerOpt: IO[Option[Player]] = {
    for {
      _       <- printPlayerInputInstructions
      input   <- IO { scala.io.StdIn.readLine }
      player  <- IO { createPlayer(input) }
      result  <- IO { player match {
        case Right(p) =>  println("Player added: " + p)
          Some(p)
        case Left(_)  =>  println("Invalid input! " + _)
          None } }
    } yield result
  }

  /**
    * Creates a Player from a string
    * @param input the string describing a Player
    * @return a Player or None
    */
  private def createPlayer(input: String): InvalidInputError Either Player = {
    // TODO Player needs two additional fields (canJump, canLift)
    input match {
      case name => Player(name).asRight
      case _    => new InvalidInputError("Could not parse: " + input).asLeft
    }
  }

}
