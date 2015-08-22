package examples.webevent

import predicates.General.ofType
import predicates.Indexes.first
import examples.webevent._
import examples.webevent.Predicates.{clicked, referredBy, currentElementContainsListing, sameShop}
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

	val PurchasedIndirectlyFromSearch = Query(
		ofType[SearchEvent],
		ofType[ListingEvent] && clicked,
		ofType[ListingEvent] && sameShop,
		ofType[PurchaseEvent] && referredBy && currentElementContainsListing
	)

	def tabbedBrowsing(series: Seq[WebEvent]): Query[WebEvent] =
		tabbedBrowsing(series.size)

	def tabbedBrowsing(seriesSize: Int): Query[WebEvent] =
		Query(
			first,
			(1 until seriesSize).map(i => !first && referredBy): _*
		)
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
