package com.tauro.creditcards

import cats.effect.Sync
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl


object CreditcardsRoutes {

  def creditCardRoutes[F[_]: Sync](C: CreditCards[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case req @ POST -> Root / "creditcards" =>
        for {
          resp <- C.creditCards(req)
        } yield resp
    }
  }
}

//No implicit arguments of type: EntityEncoder[F, List[CreditCards.CreditCard]]