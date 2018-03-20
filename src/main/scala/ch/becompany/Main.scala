package ch.becompany

import cats.Monad
import cats.implicits._
import ch.becompany.authn.Dsl.UserRepositoryState
import ch.becompany.authn.domain.AuthnError
import ch.becompany.authn.{Dsl => AuthnDsl}
import ch.becompany.email.Dsl.{Email, MailQueue}
import ch.becompany.email.{Dsl => EmailDsl}
import ch.becompany.shared.domain.{EmailAddress, User}
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import shapeless.tag

import scala.language.higherKinds

object Main extends App {

  def registerAndLogin[F[_] : Monad](implicit authnDsl: AuthnDsl[F], emailDsl: EmailDsl[F]):
      F[Either[AuthnError, User]] = {

    val email = tag[EmailAddress]("john@doe.com")
    val password = "swordfish"

    for {
      _ <- authnDsl.register(email, password)
      _ <- emailDsl.send(email, "Hello", "Thank you for registering")
      authenticated <- authnDsl.authn(email, password)
    } yield authenticated
  }

  type Stack = Fx.fx2[UserRepositoryState, MailQueue]

  val program = registerAndLogin[Eff[Stack, ?]]

  val result = program
    .runState(List.empty[User])
    .runWriterFold(ListFold[Email])
    .run

  val ((authenticated, users), emails) = result

  println("Authentiated: " + authenticated)
  println("Users: " + users)
  println("E-Mails: " + emails)
}
