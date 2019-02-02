package com.github.fbaierl.lineout

import cats.effect.IO
import com.github.fbaierl.lineout.Main.Team

package object messages {

  def printWelcomeMessage: IO[Unit] = IO(println(
    """
      |--------------------------------------
      |------ Rugby Lineout Simulator -------
      |--------------------------------------
    """.stripMargin))

  def printCustomOrPreMadeTeamQuestion: IO[Unit] = IO(println(
    """
      |--------------------------------------
      |-- Do you want to: -------------------
      |-- (1) create a custom team or -------
      |-- (2) use a pre-made team -----------
      |--------------------------------------
    """.stripMargin
  ))

  def printPlayerInputInstructions: IO[Unit] = IO(println(
    """
      |Input format:
      |<name> [jump] [lift]
      |The name of a player is mandatory and 'jump' and 'lift'
      |are optional and describe the players capabilities."
    """.stripMargin
  ))

  def printOrganizePlayersInstructions: IO[Unit] = IO(println(
    """
      |Organize your players:
      |Input format: List of numbers
      |E.g. '2 3 2' will result in: 12 345 67
      |
    """.stripMargin
  ))

  def printTeam(team: Team): IO[Unit] = IO(println(
    s"""
      |--------------------------------------
      |-- Here is your team: ----------------
      |-- (name [canJump, canLift]) ---------
      |--------------------------------------
      |
      |${team.players.map(p => p.name + " [" + (if(p.canJump) "Yes" else "No")+  ", " +  (if(p.canLift) "Yes" else "No") + "]")
          .mkString("\n")}
    """.stripMargin
  ))
}
