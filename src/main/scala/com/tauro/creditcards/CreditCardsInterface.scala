package com.tauro.creditcards

import cats.effect.Concurrent
import cats.implicits._
import io.circe.Json
import org.http4s._
import org.http4s.circe._
import com.tauro.creditcards.Protocol._


trait CreditCardsInterface[F[_]]{
  def creditCards(req: Request[F]): F[Json]
}

class CreditCardsInterfaceImpl[F[_]: Concurrent](creditCardGateway: CreditCardGateway[F], creditCardService: CreditCardService) extends CreditCardsInterface[F] {
  override def creditCards(req: Request[F]): F[Json] = {
    for {
      request <- req.as[CreditCardRequest]
      csCards <- creditCardGateway.csCards(request)
      scoredCards <- creditCardGateway.scoredCards(request)
    } yield creditCardService.creditCards(csCards, scoredCards)
  }
}

