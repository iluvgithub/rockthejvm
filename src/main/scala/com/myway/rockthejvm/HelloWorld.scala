package com.myway.rockthejvm

import cats.effect.{ExitCode, IO, IOApp}

object HelloWorld extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      msg <- sayHello("World")
      _ <-IO.println(msg)
    } yield ExitCode.Success


  def sayHello(s:String):IO[String]=IO(s"Hello, $s!")
}