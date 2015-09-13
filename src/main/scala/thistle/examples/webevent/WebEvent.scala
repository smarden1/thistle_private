package thistle.examples.webevent

trait WebEvent {
  val referrer: String
  val url: String
  val pageId: Long

  def name =
    this.getClass.getName.split("\\.").last
}

trait ListingsDisplay {
  val listingIds: Seq[Long]
}

trait HasShop {
  val shopId: Long
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
  shopId: Long) extends WebEvent with HasShop{}

case class ShopEvent(
  referrer: String,
  url: String,
  pageId: Long,
  shopId: Long) extends WebEvent with HasShop{}

case class PurchaseEvent(
  referrer: String,
  url: String,
  pageId: Long,
  listingIds: Seq[Long]) extends WebEvent with ListingsDisplay{}

case class DefaultWebEvent(
  referrer: String,
  url: String,
  pageId: Long) extends WebEvent {}