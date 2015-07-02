package examples.webevent

trait WebEvent {
	val referrer: String
	val url: String
	val pageId: Long
}

trait ListingsDisplay {
	val listingIds: Seq[Long]
}

case class SearchEvent(
	referrer: String,
	url: String,
	pageId: Long,
	query: String,
	listingIds: Seq[Long]) extends WebEvent with ListingsDisplay{}

case class ListingEvent(
	referrer: String,
	url: String,
	pageId: Long,
	listingId: Long,
	shopId: Long) extends WebEvent {}

case class PurchaseEvent(
	referrer: String,
	url: String,
	pageId: Long,
	listingIds: Seq[Long]) extends WebEvent with ListingsDisplay{}