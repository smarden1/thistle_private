package examples.webevent

import predicates.General.ofType
import predicates.Indexes.intervening
import examples.webevent._
import examples.webevent.Predicates.{clicked, referredBy, currentElementContainsListing}

object Queries {

	val PurchasedFromSearch = List(
		ofType[SearchEvent],
		ofType[ListingEvent] && clicked,
		ofType[PurchaseEvent] && referredBy && currentElementContainsListing
	)

	// val PurchasedFromSearch = List(
	// 	ofType[SearchEvent],
	// 	ofType[ListingEvent] && clicked,
	// 	ofType[PurchaseEvent] && referredBy && currentElementContainsListing
	// )

	val PurchaseChannel = List(
		ofType[ListingsDisplay],
		ofType[ListingEvent] && clicked,
		ofType[PurchaseEvent] && referredBy && currentElementContainsListing
	)

	val PostLogin = List(
		ofType[SearchEvent],
		intervening(2)
	)

	// would be nice to be able to construct arbitrary long queries to find paths
	// can make the query as long as the sequence if necessary?
}


/*
* can matchstate have a specific type for no previous matches?
* can make implicit on collections to findAll or findNext and take this type of matchState
* implicit to make collections have a match on them
* Query type where first element is this matchState and then some variable number
*
* traversing the results of a tree
* find the count of siblings?
* how many other listings from the same shop that were not purchased did we look at
* do we want to make matchTree out of any root (just drop the heads of the query at this point or make it available on all nodes)
*
* do we want to make queries take trees instead of just single paths
*
*
* some way to find endless matches so we can find tabbed browsing - maybe just extend the query to be the size of the input?
 */