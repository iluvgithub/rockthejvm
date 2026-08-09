package com.myway.rockthejvm

import cats.Id
import cats.effect.IO
import cats.effect.kernel.Outcome
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.testkit.TestControl
import munit.CatsEffectAssertions.MUnitCatsAssertionsForIOOps
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.concurrent.duration.DurationInt

class HelloWorldTest extends AsyncFunSuite with AsyncIOSpec {
  
  test("HelloWorld computes Hello, world!") {
    val program: IO[String] = HelloWorld.sayHello("world")
    program.map(msg => msg shouldBe "Hello, world!")
  }

  test(" HelloWorld with test control") {
    // arrange
    val ioProgram: IO[String] = HelloWorld.sayHello("World")

    // act
    for {
      control <- TestControl.execute(ioProgram)
      _ <- control.tick
      _ <- control.advanceAndTick(100.millis)
      // assert
      _ <- control.results.assertEquals(
        Some(Outcome.succeeded[Id, Throwable, String]("Hello, World!"))
      )
    } yield ()

  }
}
