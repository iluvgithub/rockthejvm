package com.myway.rockthejvm.stream

import cats.effect.IO
import cats.effect.kernel.Outcome
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.testkit.TestControl
import fs2.Stream
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.concurrent.duration.{DurationInt, _}

class StreamTestControlTest extends AsyncFunSuite with AsyncIOSpec {

  test("should emit values at the expected times") {

    val helloStream: Stream[IO, String] =
      Stream.constant("hello").covary[IO].metered(5.seconds).take(2)

    val streamWithTime: Stream[IO, (FiniteDuration, String)] =
      Stream.eval(IO.monotonic).flatMap { t0 =>
        helloStream.evalMap { value =>
          IO.monotonic.map(t1 => (t1 - t0, value))
        }
      }

    val program: IO[Vector[(FiniteDuration, String)]] =
      streamWithTime.compile.toVector

    // Run under TestControl so time advances deterministically and instantly
    TestControl.executeEmbed(program).asserting { result =>
      result shouldBe Vector(
        5.seconds -> "hello",
        10.seconds -> "hello"
      )
    }
  }

  test("can also be stepped manually with TestControl") {
    val stream: Stream[IO, Int] =
      Stream
        .iterate(0)(_ + 1)
        .covary[IO]
        .metered(1.second)
        .take(3)

    val program = stream.compile.toList

    TestControl.execute(program).flatMap { ctrl =>
      for {
        _ <- ctrl.tick // Kick off the stream
        _ <- ctrl.results
          .asserting(_ shouldBe None) // Nothing has been emitted yet
        _ <- ctrl.advance(
          1.second
        ) // Advance 1 second → first element becomes available
        _ <-
          ctrl.tick // (results still None because we haven't finished the whole program)
        _ <- ctrl.tickAll // Advance the remaining time and run to completion
        result <- ctrl.results
      } yield {
        result shouldBe Some(Outcome.succeeded(List(0, 1, 2)))
      }
    }
  }

}
