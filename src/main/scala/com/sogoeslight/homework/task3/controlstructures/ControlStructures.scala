package com.sogoeslight.homework.task3.controlstructures

import java.text.DecimalFormat

import scala.io.Source

object ControlStructures {
  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:
  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"
  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"
  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"
  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"
  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class Result(command: Command, value: Double)

  final case class ErrorMessage(value: String) {
    override def toString: String = "Error: " + value
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    import Command._

    val input: List[String] = x.trim.toLowerCase().split(" ").toList

    input match {
      case List("")                                           => Left(ErrorMessage("Empty input given"))
      case _ :: xs if xs.map(_.toDoubleOption).contains(None) => Left(ErrorMessage("Invalid arguments. Failed parsing numbers"))
      case x :: xs =>
        x match {
          case _ if xs.length < 2        => Left(ErrorMessage("Not enough arguments"))
          case "divide" if xs.length > 2 => Left(ErrorMessage("Division takes only 2 arguments"))
          case "divide"                  => Right(Divide(xs.head.toDouble, xs.tail.head.toDouble))
          case "sum"                     => Right(Sum(xs.map(_.toDouble)))
          case "average"                 => Right(Average(xs.map(_.toDouble)))
          case "min"                     => Right(Min(xs.map(_.toDouble)))
          case "max"                     => Right(Max(xs.map(_.toDouble)))
          case _                         => Left(ErrorMessage("Invalid command"))
        }
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    import Command._

    x match {
      case Divide(_, 0)              => Left(ErrorMessage("Can not divide by 0"))
      case Divide(dividend, divisor) => Right(Result(x, dividend / divisor))
      case Sum(xs)                   => Right(Result(x, xs.sum))
      case Average(xs)               => Right(Result(x, xs.sum / xs.length))
      case Min(xs)                   => Right(Result(x, xs.min))
      case Max(xs)                   => Right(Result(x, xs.max))
      case _                         => Left(ErrorMessage("Invalid command"))
    }
  }

  def renderResult(result: Result): String = {
    import Command._

    val formatter = new DecimalFormat("#.###")

    result.command match {
      case Divide(dividend, divisor) =>
        s"${formatter.format(dividend)} divided by ${formatter.format(divisor)} is ${formatter.format(result.value)}"
      case Sum(numbers) =>
        s"the sum of ${numbers.map(x => formatter.format(x)).mkString(" ")} is ${formatter.format(result.value)}"
      case Average(numbers) =>
        s"the average of ${numbers.map(x => formatter.format(x)).mkString(" ")} is ${formatter.format(result.value)}"
      case Min(numbers) =>
        s"the minimum of ${numbers.map(x => formatter.format(x)).mkString(" ")} is ${formatter.format(result.value)}"
      case Max(numbers) =>
        s"the maximum of ${numbers.map(x => formatter.format(x)).mkString(" ")} is ${formatter.format(result.value)}"
    }
  }

  def process(x: String): String = {
    val output: Either[ErrorMessage, Result] = for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result

    output match {
      case Right(result) => renderResult(result)
      case Left(error)   => s"$error"
    }
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
