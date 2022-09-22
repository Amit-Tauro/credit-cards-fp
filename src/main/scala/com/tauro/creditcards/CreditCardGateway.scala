package com.tauro.creditcards

import cats.effect.Concurrent
import cats.implicits._
import io.circe.Json
import org.http4s._
import org.http4s.implicits._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.Method._
import com.tauro.creditcards.Protocol._

final case class CsCardError(e: Throwable) extends RuntimeException

final case class ScoredCardError(e: Throwable) extends RuntimeException


trait CreditCardGateway[F[_]] {
  def scoredCards(req: CreditCardRequest)(implicit decoder: EntityDecoder[F, Json]): F[Json]
  def csCards(req: CreditCardRequest)(implicit decoder: EntityDecoder[F, Json]): F[Json]
}

class CreditCardGatewayImpl[F[_]: Concurrent](C: Client[F]) extends CreditCardGateway[F] {
  val dsl = new Http4sClientDsl[F]{}
  import dsl._

  override def csCards(req: CreditCardRequest)(implicit decoder: EntityDecoder[F, Json]): F[Json] = {
    val csReq: Request[F] = POST(uri"https://app.clearscore.com/api/global/backend-tech-test/v1/cards").
        withEntity(CsCardRequest(req.name, req.creditScore))
    C.expect[Json](csReq).adaptError{ case t => CsCardError(t)}// Prevent Client Json Decoding Failure Leaking
  }

  override def scoredCards(req: CreditCardRequest)(implicit decoder: EntityDecoder[F, Json]): F[Json] = {
    val scReq: Request[F] = POST(uri"https://app.clearscore.com/api/global/backend-tech-test/v2/creditcards").withEntity(
      ScoredCardsRequest(req.name, req.creditScore, req.salary))
    C.expect[Json](scReq).adaptError{ case t => ScoredCardError(t)}// Prevent Client Json Decoding Failure Leaking
  }
}