package thistle.examples.webevent

import thistle.core.Query

import thistle.predicates.General.ofType
import thistle.predicates.Indexes.first
import thistle.examples.webevent._
import thistle.examples.webevent.Predicates.{clicked, referredBy, currentElementContainsListing, sameShop}

object Queries {

  val PurchasedFromSearch = Query(
    ofType[SearchEvent],
    ofType[ListingEvent] && clicked,
    ofType[PurchaseEvent] && currentElementContainsListing
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