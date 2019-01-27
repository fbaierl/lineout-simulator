package com.github.fbaierl.lineout

import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Main extends IOApp {

  class InvalidInputError(msg: String)

  case class Player (name: String)
  case class Team (players: List[Player])
  case class LineoutState ()

  def run(args: List[String]): IO[ExitCode] = {
    val program = for {
      _     <- liftIoIntoStateT (IO { println("-------------------------------") })
      _     <- liftIoIntoStateT (IO { println("--- Rugby Lineout Simulator ---") })
      _     <- liftIoIntoStateT (IO { println("-------------------------------") })
      _     <- liftIoIntoStateT (IO { println("First add 7 players to your team. ") })
      team  <- readTeam()
      _     <- liftIoIntoStateT (IO { println("Here is your team: " + team) })

    } yield ExitCode.Success

    val t = program.run(Team(Nil)).unsafeRunSync()

    println("Here is the team so far: " + t._1)

    IO(t._2)
  }

  def addPlayerWithStateT(newValue: Player): StateT[IO, Team, Player] = StateT { oldState: Team =>
    val newState: Team = oldState.copy(players = oldState.players :+ newValue)
    IO(newState, newValue)
  }

  def readTeam(): StateT[IO, Team, Player] = {
      for {
        player <- liftIoIntoStateT(readPlayer())
        team   <- if (player.isEmpty) {
                    liftIoIntoStateT(IO(println("WRONG INPUT - ENDING GAME")))  // quit on wrong input

                  } else for {
                    _ <- addPlayerWithStateT(player.get)
                    _ <- readTeam()
                  } yield Unit
      } yield team
  }

  /**
    * the purpose of this function is to “lift” an IO action into the StateT monad.
    * given an IO instance named `io` as input, the anonymous function transforms
    * the `IO[A]` into an `IO[(Team, A)]`; that result is then wrapped in a `StateT`.
    */
  def liftIoIntoStateT[A](io: IO[A]): StateT[IO, Team, A] = StateT { s => io.map(a => (s, a)) }

  /**
    * Reads a player from the command-line
    * @return the player or None
    */
  private def readPlayer(): IO[Option[Player]] = {
    for {
      _       <- IO { println("Input format:") }
      _       <- IO { println("<name> [jump] [lift]") }
      _       <- IO { println("The name of a player is mandatory and 'jump' and 'lift' are optional and describe the players capabilities.") }
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
