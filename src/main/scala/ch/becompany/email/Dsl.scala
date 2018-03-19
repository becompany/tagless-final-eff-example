package ch.becompany.email

import cats.data.Writer
import ch.becompany.shared.domain.EmailAddress
import org.atnos.eff.{Eff, |=}
import org.atnos.eff.WriterEffect
import shapeless.tag.@@

import scala.language.higherKinds

trait Dsl[F[_]] {

  def send(to: String @@ EmailAddress, subject: String, body: String): F[Unit]

}

object Dsl {

  final case class Email(to: String @@ EmailAddress, subject: String, body: String)

  type MailQueue[A] = Writer[Email, A]

  type _mailQueue[R] = MailQueue |= R

  implicit def writerEffInterpreter[R : _mailQueue]: Dsl[Eff[R, ?]] =
    new Dsl[Eff[R, ?]] {
      override def send(to: @@[String, EmailAddress], subject: String, body: String): Eff[R, Unit] =
        WriterEffect.tell(Email(to, subject, body))
    }

}
