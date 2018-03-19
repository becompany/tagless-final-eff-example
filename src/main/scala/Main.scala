import cats.Monad
import cats.implicits._
import ch.becompany.authn.{Dsl => AuthnDsl}
import ch.becompany.email.{Dsl => EmailDsl}
import ch.becompany.shared.domain.EmailAddress
import shapeless.tag

import scala.language.higherKinds

object Main extends App {

  def program[F[_] : Monad](authnDsl: AuthnDsl[F], emailDsl: EmailDsl[F]): F[Unit] = {

    val email = tag[EmailAddress]("john@doe.com")
    val password = "swordfish"

    for {
      userE <- authnDsl.register(email, password)
      _ <- authnDsl.authn(email, password)
    } yield ()
  }

}
