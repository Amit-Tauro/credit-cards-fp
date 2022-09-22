package com.tauro.creditcards

import io.circe.Json
import io.circe.syntax._
import com.tauro.creditcards.Protocol._

trait CreditCardService {
  def creditCards(cs: Json, sc: Json): Json
}

class CreditCardServiceImpl extends CreditCardService {

  override def creditCards(cs: Json, sc: Json): Json = {
    val csList: List[CsCardResponse] = cs.as[List[CsCardResponse]].getOrElse(List.empty)
    val scList: List[ScoredCardsResponse] = sc.as[List[ScoredCardsResponse]].getOrElse(List.empty)
    val creditCardCsList: List[CreditCard] = csList.map(r => CreditCard(
      provider = "CSCards", name = r.cardName, apr = r.apr, cardScore = CsScore(r)))
    val creditCardScList: List[CreditCard] = scList.map(r => CreditCard(
      provider = "ScoredCards", name = r.card, apr = r.apr, cardScore = ScScore(r)))
    val combinedList: List[CreditCard] = creditCardCsList ::: creditCardScList
    val sortList: List[CreditCard] = combinedList.sortWith(sortingScore)
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

// todo remove getOrElse
