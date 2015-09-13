package thistle.core

case class MatchSequence[T](sequence: Seq[T]) {
  private lazy implicit val series =
    sequence.toVector

  def filter(ep: ElementPredicate[T]) =
    series
      .view
      .zipWithIndex
      .filter{case(k, i) => ep(new ElementState(i){override lazy val value = k})}
      .map(_._1)
      .force

  def filterNot(ep: ElementPredicate[T]) =
    filter(!ep)

  def exists(ep: ElementPredicate[T]): Boolean =
    series
      .zipWithIndex
      .exists{case(k, i) => ep(ElementState(i))}

  def forall(ep: ElementPredicate[T]) =
    !exists(!ep)

  def count(ep: ElementPredicate[T]): Int =
    filter(ep).length

  def find(ep: ElementPredicate[T]): Option[T] =
    series
      .view
      .zipWithIndex
      .find{case(k, i) => ep(new ElementState(i){override lazy val value = k})}
      .map(_._1)

  // is there at least one completed query?
  def exists(q: Query[T]): Boolean =
    !MatchTreeBuilder(q).allCompleteMatches.isEmpty
}

trait MatchSequenceImplicits {
  implicit def sequence2MatchSequence[T](seq: Seq[T]): MatchSequence[T] =
    MatchSequence(seq)

  implicit def matchSequence2Sequence[T](matchSequence: MatchSequence[T]): Seq[T] =
    matchSequence.sequence
}