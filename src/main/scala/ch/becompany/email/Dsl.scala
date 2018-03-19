package ch.becompany.email

import ch.becompany.shared.domain.EmailAddress
import shapeless.tag.@@

import scala.language.higherKinds

trait Dsl[F[_]] {

  def send(email: String @@ EmailAddress, subject: String, body: String): F[Unit]

}