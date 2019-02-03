package com.github.fbaierl.lineout

import cats.data.Validated
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import messages._

object Main extends IOApp {

  class InvalidInputError(msg: String)
  case class Player (name: String, canJump: Boolean, canLift: Boolean)
  case class Lineout (positions: List[Option[Player]])
  case class Formation (groups: List[Int])
  case class Maneuver (moves: List[Move])
  case class Move (player: Player, action: Action)
  abstract class Action
  case class Jump(isFake: Boolean) extends Action
  case class GoTo(position: Int) extends Action
  case class ManeuverInvalid(errorMessage: String)

  def premadeTeam: List[Player] =
    Player("Evan", canJump = true, canLift = false) ::
    Player("Sang", canJump = true, canLift = true) ::
    Player("Duncan", canJump = false, canLift = true) ::
    Player("Lyle", canJump = false, canLift = true) ::
    Player("Barney", canJump = false, canLift = true) ::
    Player("Dick", canJump = false, canLift = true) ::
    Player("Franklyn", canJump = true, canLift = true) :: Nil

  def run(args: List[String]): IO[ExitCode] = for {
      _         <- printWelcomeMessage
      custom    <- readCustomOrPreMadeTeamQuestion
      team      <- if(custom) readTeam else IO(premadeTeam)
      _         <- printTeam(team)
      _         <- printOrganizePlayersInstructions
      orga      <- readOrganizePlayersCommand
      lineout   <- IO { createLineOut(team, orga) }
      maneuver  <- readManeuver // TODO more than one maneuver is possible
      valid     <- validateManeuver(maneuver, lineout)
      _         <- if(valid) printManeuver(maneuver) else IO { println("TODO") }
    } yield ExitCode.Success

  /**
    * Asks the user if he wants to build a custom team or use a premade one.
    * @return true, if the user decided to use a custom team
    */
  def readCustomOrPreMadeTeamQuestion: IO[Boolean] = for {
    _       <- printCustomOrPreMadeTeamQuestion
    input   <- IO { scala.io.StdIn.readLine == "1" }
  } yield input


  def readManeuver: IO[Maneuver] = ???

  def createLineOut(team: List[Player], orga: List[Int]): Lineout = ???

  def readOrganizePlayersCommand: IO[List[Int]] = ???

  /**
    * Reads a team of 7 players from the command line.
    * @return
    */
  def readTeam: IO[List[Player]] =
    (Nil: List[Player]).iterateUntilM[IO](ps => readPlayer.map(p => ps :+ p))(_.size >= 7)

  /**
    * Reads a player from the command-line
    * @return the player
    */
  private def readPlayer: IO[Player] =
    (None: Option[Player]).iterateUntilM[IO](_ => readPlayerOpt)(p => p.isDefined).map(_.get)

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

  private def validateManeuver(man: Maneuver, initialLineout: Lineout): Validated[ManeuverInvalid, Maneuver]= ???

}
