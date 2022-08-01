package com.tauro.creditcards

import cats.effect.Sync
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._


object CreditcardsRoutes {

  def helloWorldRoutes[F[_]: Sync](H: HelloWorld[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        for {
          greeting <- H.hello(HelloWorld.Name(name))
          resp <- Ok(greeting)
        } yield resp
    }
  }

  def creditCardRoutes[F[_]: Sync](C: CreditCards[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case req @ POST -> Root / "creditcards" =>
        for {
          request <- C.getRequest(req)
          csResponse <- C.csCards(request)
          scResponse <- C.scoredCards(request)
          resp <- Ok(C.calculate(csResponse, scResponse))
        } yield resp

      case req @ POST -> Root / "test" =>
        for {
          request <- C.getRequest(req)
          csResponse <- C.success(request)
          resp <- Ok(csResponse.toString)
        } yield resp
    }
  }
}

//No implicit arguments of type: EntityEncoder[F, List[CreditCards.CreditCard]]