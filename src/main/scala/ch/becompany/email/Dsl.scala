package ch.becompany.email

import cats.data.Writer
import ch.becompany.shared.domain.EmailAddress
import org.atnos.eff.{Eff, |=}
import shapeless.tag.@@

import scala.language.higherKinds

trait Dsl[F[_]] {

  def send(to: String @@ EmailAddress, subject: String, body: String): F[Unit]

}

object Dsl {

  final case class Email(to: String @@ EmailAddress, subject: String, body: String)

  type MailQueue[A] = Writer[Email, A]

  implicit lazy val writerInterpreter: Dsl[MailQueue] =
    new Dsl[MailQueue] {
      override def send(to: @@[String, EmailAddress], subject: String, body: String): MailQueue[Unit] =
        Writer.tell(Email(to, subject, body))
    }

  private def effInterpreter[R, F[_]](interpreter: Dsl[F])(implicit ev: |=[F, R]): Dsl[Eff[R, ?]] =
    new Dsl[Eff[R, ?]] {
      override def send(to: @@[String, EmailAddress], subject: String, body: String): Eff[R, Unit] =
        Eff.send(interpreter.send(to, subject, body))
    }

  type _mailQueue[R] = MailQueue |= R

  implicit def effWriterInterpreter[R : _mailQueue]: Dsl[Eff[R, ?]] =
    effInterpreter[R, MailQueue](writerInterpreter)

}
