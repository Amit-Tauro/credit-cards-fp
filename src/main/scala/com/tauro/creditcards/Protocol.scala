package com.tauro.creditcards

import cats.effect.Concurrent
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}

object Protocol {

  final case class CreditCardRequest(name: String, creditScore: Int, salary: Int)
  object CreditCardRequest {
    implicit val creditCardRequestDecoder: Decoder[CreditCardRequest] = deriveDecoder[CreditCardRequest]
    implicit def creditCardRequestEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, CreditCardRequest] =
      jsonOf
  }

  final case class CsCardRequest(name: String, creditScore: Int)

  object CsCardRequest {
    implicit val csCardRequestEncoder: Encoder[CsCardRequest] = deriveEncoder[CsCardRequest]
    implicit def csCardRequestEntityEncoder[F[_]]: EntityEncoder[F, CsCardRequest] =
      jsonEncoderOf
  }

  final case class CsCardResponse(cardName: String, apr: Double, eligibility: Double)


  object CsCardResponse {
    implicit val csCardResponseDecoder: Decoder[CsCardResponse] = deriveDecoder[CsCardResponse]
    implicit def csCardResponseEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, CsCardResponse] =
      jsonOf
  }

  final case class ScoredCardsRequest(name: String, score: Int, salary: Int)

  object ScoredCardsRequest {
    implicit val scoredCardsRequestEncoder: Encoder[ScoredCardsRequest] = deriveEncoder[ScoredCardsRequest]
    implicit def scoredCardsRequestEntityEncoder[F[_]]: EntityEncoder[F, ScoredCardsRequest] =
      jsonEncoderOf
  }

  final case class ScoredCardsResponse(card: String, apr: Double, approvalRating: Double)

  object ScoredCardsResponse {
    implicit val scoredCardsResponseDecoder: Decoder[ScoredCardsResponse] = deriveDecoder[ScoredCardsResponse]
    implicit def scoredCardsResponseEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, ScoredCardsResponse] =
      jsonOf
  }

  final case class CreditCard(provider: String, name: String, apr: Double, cardScore: Double)

  object CreditCard {
    implicit val creditCardEncoder: Encoder[CreditCard] = deriveEncoder[CreditCard]
    implicit def creditCardEntityEncoder[F[_]]: EntityEncoder[F, CreditCard] =
      jsonEncoderOf
  }

}
