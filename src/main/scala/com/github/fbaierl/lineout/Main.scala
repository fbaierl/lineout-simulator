package com.github.fbaierl.lineout

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import messages._

object Main extends IOApp {

  class InvalidInputError(msg: String)
  case class Player (name: String, canJump: Boolean, canLift: Boolean)
  case class Team (players: List[Player])
  case class Formation (groups: List[Int])

  def premadeTeam: Team = Team(
    Player("Evan", canJump = true, canLift = true) ::
    Player("Sang", canJump = true, canLift = true) ::
    Player("Duncan", canJump = true, canLift = true) ::
    Player("Lyle", canJump = true, canLift = true) ::
    Player("Barney", canJump = true, canLift = true) ::
    Player("Dick", canJump = true, canLift = true) ::
    Player("Franklyn", canJump = true, canLift = true) :: Nil
  )

  def run(args: List[String]): IO[ExitCode] = for {
      _       <- printWelcomeMessage
      custom <- readCustomOrPreMadeTeamQuestion
      team    <- if(custom) readTeam else IO(premadeTeam)
      _       <- printTeam(team)
      _       <- printOrganizePlayersInstructions
    } yield ExitCode.Success

  /**
    * Asks the user if he wants to build a custom team or use a premade one.
    * @return true, if the user decided to use a custom team
    */
  def readCustomOrPreMadeTeamQuestion: IO[Boolean] = for {
    _       <- printCustomOrPreMadeTeamQuestion
    input   <- IO { scala.io.StdIn.readLine == "1" }
  } yield input

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
        case Left(_)  =>  println("Invalid input!")
          None } }
    } yield result
  }

  /**
    * Creates a Player from a string
    * @param input the string describing a Player
    * @return a Player or None
    */
  private def createPlayer(input: String): InvalidInputError Either Player = {
    input.toLowerCase.split(" ").toList match {
      case name :: attributes if !name.isEmpty =>
        Player(name, attributes.contains("jump"), attributes.contains("lift")).asRight
      case _    =>
        new InvalidInputError("Could not parse: " + input).asLeft
    }
  }

}
