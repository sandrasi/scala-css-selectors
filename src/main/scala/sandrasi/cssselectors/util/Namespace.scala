package sandrasi.cssselectors.util

sealed trait Namespace
case object NoNamespace extends Namespace
case class UriNamespace(uri: String) extends Namespace
case object AnyNamespace extends Namespace
case object InvalidNamespace extends Namespace
