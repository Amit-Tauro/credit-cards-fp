package com.tauro.creditcards

import cats.effect.Concurrent
import cats.implicits._
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto._
import org.http4s._
import org.http4s.implicits._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.circe._
import org.http4s.Method._
import io.circe.syntax._


trait
CreditCards[F[_]]{
  def get: F[CreditCards.CreditCardRequest]
  def getRequest(req: Request[F]): F[CreditCards.CreditCardRequest]
  def csCards(req: CreditCards.CreditCardRequest): F[Json]
  def scoredCards(req: CreditCards.CreditCardRequest): F[Json]
  def calculate(cs: Json, sc: Json): Json
  def success(req: CreditCards.CreditCardRequest): F[Boolean]
}

object CreditCards {
  def apply[F[_]](implicit ev: CreditCards[F]): CreditCards[F] = ev

  final case class CreditCardRequest(name: String, creditScore: Int, salary: Int)
  object CreditCardRequest {
    implicit val creditCardRequestDecoder: Decoder[CreditCardRequest] = deriveDecoder[CreditCardRequest]
    implicit def creditCardRequestEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, CreditCardRequest] =
      jsonOf
    implicit val creditCardRequestEncoder: Encoder[CreditCardRequest] = deriveEncoder[CreditCardRequest]
    implicit def creditCardRequestEntityEncoder[F[_]]: EntityEncoder[F, CreditCardRequest] =
      jsonEncoderOf
  }

  final case class CsCardRequest(name: String, creditScore: Int)

  object CsCardRequest {
    implicit val csCardRequestDecoder: Decoder[CsCardRequest] = deriveDecoder[CsCardRequest]
    implicit def csCardRequestEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, CsCardRequest] =
      jsonOf
    implicit val csCardRequestEncoder: Encoder[CsCardRequest] = deriveEncoder[CsCardRequest]
    implicit def csCardRequestEntityEncoder[F[_]]: EntityEncoder[F, CsCardRequest] =
      jsonEncoderOf
  }

  final case class CsCardResponse(cardName: String, apr: Double, eligibility: Double)


  object CsCardResponse {
    implicit val csCardResponseDecoder: Decoder[CsCardResponse] = deriveDecoder[CsCardResponse]
    implicit def csCardResponseEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, CsCardResponse] =
      jsonOf
    implicit val csCardResponseEncoder: Encoder[CsCardResponse] = deriveEncoder[CsCardResponse]
    implicit def csCardResponseEntityEncoder[F[_]]: EntityEncoder[F, CsCardResponse] =
      jsonEncoderOf

  }

  final case class ScoredCardsRequest(name: String, score: Int, salary: Int)

  object ScoredCardsRequest {
    implicit val scoredCardsRequestDecoder: Decoder[ScoredCardsRequest] = deriveDecoder[ScoredCardsRequest]
    implicit def scoredCardsRequestEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, ScoredCardsRequest] =
      jsonOf
    implicit val scoredCardsRequestEncoder: Encoder[ScoredCardsRequest] = deriveEncoder[ScoredCardsRequest]
    implicit def scoredCardsRequestEntityEncoder[F[_]]: EntityEncoder[F, ScoredCardsRequest] =
      jsonEncoderOf
  }

  final case class ScoredCardsResponse(card: String, apr: Double, approvalRating: Double)

  object ScoredCardsResponse {
    implicit val scoredCardsResponseDecoder: Decoder[ScoredCardsResponse] = deriveDecoder[ScoredCardsResponse]
    implicit def scoredCardsResponseEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, ScoredCardsResponse] =
      jsonOf
    implicit val scoredCardsResponseEncoder: Encoder[ScoredCardsResponse] = deriveEncoder[ScoredCardsResponse]
    implicit def scoredCardsResponseEntityEncoder[F[_]]: EntityEncoder[F, ScoredCardsResponse] =
      jsonEncoderOf
  }

  final case class CreditCard(provider: String, name: String, apr: Double, cardScore: Double)

  object CreditCard {
    implicit val creditCardDecoder: Decoder[CreditCard] = deriveDecoder[CreditCard]
    implicit def creditCardEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, CreditCard] =
      jsonOf
    implicit val creditCardEncoder: Encoder[CreditCard] = deriveEncoder[CreditCard]
    implicit def creditCardEntityEncoder[F[_]]: EntityEncoder[F, CreditCard] =
      jsonEncoderOf
  }

  final case class CreditCardRequestError(e: Throwable) extends RuntimeException

  final case class CsCardError(e: Throwable) extends RuntimeException

  final case class ScoredCardError(e: Throwable) extends RuntimeException

  def impl[F[_]: Concurrent](C: Client[F]): CreditCards[F] = new CreditCards[F]{
    val dsl = new Http4sClientDsl[F]{}
    import dsl._
    def get: F[CreditCards.CreditCardRequest] = {
      C.expect[CreditCardRequest](GET(uri"https://icanhazdadjoke.com/"))
        .adaptError{ case t => CreditCardRequestError(t)} // Prevent Client Json Decoding Failure Leaking
    }

    def getRequest(req: Request[F]): F[CreditCards.CreditCardRequest] = {
      req.as[CreditCardRequest]
    }

    def csCards(req: CreditCards.CreditCardRequest): F[Json] = {
      val csReq: Request[F] = POST(uri"https://app.clearscore.com/api/global/backend-tech-test/v1/cards").withEntity(
        CsCardRequest(req.name, req.creditScore)
      )
      C.successful(csReq)
      C.expect[Json](csReq)
        .adaptError{ case t => CsCardError(t)}// Prevent Client Json Decoding Failure Leaking
    }

    def success(req: CreditCards.CreditCardRequest): F[Boolean] = {
      val csReq: Request[F] = POST(uri"https://app.clearscore.com/api/global/backend-tech-test/v1/cards").withEntity(
        CsCardRequest(req.name, req.creditScore)
      )
      C.successful(csReq)
    }

    def scoredCards(req: CreditCards.CreditCardRequest): F[Json] = {
      val scReq: Request[F] = POST(uri"https://app.clearscore.com/api/global/backend-tech-test/v2/creditcards").withEntity(
        ScoredCardsRequest(req.name, req.creditScore, req.salary)
      )
      C.expect[Json](scReq)
        .adaptError{ case t => ScoredCardError(t)}// Prevent Client Json Decoding Failure Leaking
    }

    def calculate(cs: Json, sc: Json): Json = {
      val csList: List[CsCardResponse] = cs.as[List[CsCardResponse]].getOrElse(List.empty)
      val scList: List[ScoredCardsResponse] = sc.as[List[ScoredCardsResponse]].getOrElse(List.empty)
      val creditCardCsList: List[CreditCard] = csList.map(r => CreditCard(
        provider = "CSCards", name = r.cardName, apr = r.apr, cardScore = CsScore(r)))
      val creditCardScList: List[CreditCard] = scList.map(r => CreditCard(
        provider = "ScoredCards", name = r.card, apr = r.apr, cardScore = ScScore(r)))
      val combList: List[CreditCard] = creditCardCsList ::: creditCardScList
      val sortList: List[CreditCard] = combList.sortWith(sortingScore)
      sortList.asJson
    }

    private def CsScore(cs: CsCardResponse): Double = {
      cs.eligibility * (Math.pow(1 / cs.apr, 2))
    }

    private def ScScore(sc: ScoredCardsResponse): Double = {
      val eligibility: Double = sc.approvalRating*10
      eligibility * (Math.pow(1 / sc.apr, 2))
    }

    private def sortingScore(cs1: CreditCard, cs2: CreditCard): Boolean = {
      cs1.cardScore > cs2.cardScore
    }

  }
}

//No implicit arguments of type: EntityEncoder[F, List[CreditCards.CreditCard]]

//sortingScore = eligibility ∗ ((1/apr)2)

// how to handle case implicits quickly
