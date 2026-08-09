package com.myway.rockthejvm

import cats.effect.{ExitCode, IO}
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.testkit.TestConsole
import org.scalatest.funsuite.AsyncFunSuite
import java.io.ByteArrayOutputStream

class HelloWorldTest
  extends AsyncFunSuite
    with AsyncIOSpec {
  test("HelloWorld computes Hello, world!") {

    val program: IO[String] = HelloWorld.sayHello("world")
    program.map(
       msg => msg shouldBe "Hello, world!"
    )
  }
  test("HelloWorld prints Hello, World!") {

    TestConsole[IO].flatMap { console =>

      given Console[IO] = console

      HelloWorld.program[IO] >>
        console.readOutput.map { output =>
          assert(output == List("Hello, World!"))
        }
    }
  }
}