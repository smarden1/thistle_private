package examples.webevent

import core.{MatchState, MatchPredicate, MatchPredicateImplicits, ElementState}

object Predicates  {

	val referredBy : MatchPredicate[WebEvent] = {
		m : MatchState[WebEvent] =>
			m.value.referrer == m.previousMatchedValue.url
	}

	val samePage : MatchPredicate[WebEvent] = {
		m : MatchState[WebEvent] =>
			m.value.pageId == m.previousMatchedValue.pageId
	}

	val prevMatchContainsListing : MatchPredicate[WebEvent] = {
		m : MatchState[WebEvent] =>
			((m.previousMatchedValue, m.value) match {
				case (prev: ListingsDisplay, cur: ListingEvent) => prev.listingIds.contains(cur.listingId)
				case _ => false
			})
	}

	val currentElementContainsListing : MatchPredicate[WebEvent] = {
		m : MatchState[WebEvent] =>
			((m.previousMatchedValue, m.value) match {
				case (cur: ListingsDisplay, prev: ListingEvent) => cur.listingIds.contains(prev.listingId)
				case _ => false
			})
	}

	val clicked =
		referredBy && prevMatchContainsListing

	// absract out previous matched event property matches this property
	// wish we had message passing
}