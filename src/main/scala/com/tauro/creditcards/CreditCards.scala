package com.tauro.creditcards

import cats.effect.Concurrent
import cats.implicits._
import io.circe.Json
import org.http4s._
import org.http4s.circe._
import com.tauro.creditcards.Protocol._


trait CreditCards[F[_]]{
  def creditCards(req: Request[F]): F[Json]
}

object CreditCards {
  def apply[F[_]](implicit ev: CreditCards[F]): CreditCards[F] = ev

  def impl[F[_]: Concurrent](creditCardGateway: CreditCardGateway[F],
                             creditCardService: CreditCardService): CreditCards[F] = new CreditCards[F]{

    def creditCards(req: Request[F]): F[Json] = {
      for {
        request <- getRequest(req)
        csCards <- creditCardGateway.csCards(request)
        scoredCards <- creditCardGateway.scoredCards(request)
      } yield creditCardService.creditCards(csCards, scoredCards)
    }

    private def getRequest(req: Request[F]): F[CreditCardRequest] = {
      req.as[CreditCardRequest]
    }
  }
}
