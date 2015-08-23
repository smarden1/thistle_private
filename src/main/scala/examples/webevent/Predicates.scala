package examples.webevent

import core.{MatchState, MatchPredicate, MatchPredicateImplicits, ElementState}
import predicates.Matches

object Predicates  {

	val referredBy: MatchPredicate[WebEvent] = {
		m: MatchState[WebEvent] =>
			m.value.referrer == m.previousMatchedValue.url
	}

	val samePage: MatchPredicate[WebEvent] = {
		m: MatchState[WebEvent] =>
			m.value.pageId == m.previousMatchedValue.pageId
	}

	val prevMatchContainsListing: MatchPredicate[ListingsDisplay] = {
		Matches.comparePreviousMatch(
			(prev: ListingsDisplay, cur: ListingEvent) =>
				prev.listingIds.contains(cur.listingId)
		)
	}

	val currentElementContainsListing: MatchPredicate[ListingEvent] =
		Matches.comparePreviousMatch(
			(prev: ListingEvent, cur: ListingsDisplay) =>
				cur.listingIds.contains(prev.listingId)
		)

	val clicked =
		referredBy && prevMatchContainsListing

	val sameShop =
		Matches.comparePreviousMatch(
			(prev: HasShop, cur: HasShop) =>
				cur.shopId == prev.shopId
		)
}
