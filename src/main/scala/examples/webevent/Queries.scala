package examples.webevent

import predicates.General.ofType
//import predicates.Indexes.intervening
import examples.webevent._
import examples.webevent.Predicates.{clicked, referredBy, currentElementContainsListing}
import core.Query

object Queries {

	val PurchasedFromSearch = Query(
		ofType[SearchEvent],
		ofType[ListingEvent] && clicked,
		ofType[PurchaseEvent] && referredBy && currentElementContainsListing
	)

	val PurchaseChannel = Query(
		ofType[ListingsDisplay],
		ofType[ListingEvent] && clicked,
		ofType[PurchaseEvent] && referredBy && currentElementContainsListing
	)

	val PostLogin = Query(
		ofType[SearchEvent]
		//intervening(2)
	)

	// would be nice to be able to construct arbitrary long queries to find paths
	// can make the query as long as the sequence if necessary?
}


/*
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
