package ch.becompany.authn

import cats.data.State
import cats.implicits._
import ch.becompany.authn.domain.{AuthnError, RegistrationError}
import ch.becompany.shapelessext._
import ch.becompany.shared.domain.{EmailAddress, User}
import org.atnos.eff.{Eff, |=}
import shapeless.tag.@@

import scala.language.higherKinds

trait Dsl[F[_]] {

  def register(email: String @@ EmailAddress, password: String):
      F[Either[RegistrationError, User]]

  def authn(email: String @@ EmailAddress, password: String):
      F[Either[AuthnError, User]]

}

object Dsl {

  type UserRepository = List[User]

  type UserRepositoryState[A] = State[UserRepository, A]

  implicit def stateInterpreter[R : _userRepositoryState]: Dsl[UserRepositoryState] =
    new Dsl[UserRepositoryState] {

      override def register(email: @@[String, EmailAddress], password: String):
          UserRepositoryState[Either[RegistrationError, User]] =
        for {
          users <- State.get[UserRepository]
          result <- (
            if (users.exists(_.email === email))
              State.pure(RegistrationError("User already exists").asLeft)
            else {
              val user = User(email, password)
              State((users: UserRepository) => (users :+ user, user.asRight))
            }): UserRepositoryState[Either[RegistrationError, User]]
        } yield result

      override def authn(email: @@[String, EmailAddress], password: String):
          UserRepositoryState[Either[AuthnError, User]] =
        State.inspect(_
          .find(user => user.email === email && user.password === password)
          .toRight(AuthnError("Authentication failed")))
    }

  private def effInterpreter[R, F[_]](interpreter: Dsl[F])(implicit ev: |=[F, R]): Dsl[Eff[R, ?]] =
    new Dsl[Eff[R, ?]] {

      override def register(email: @@[String, EmailAddress], password: String):
          Eff[R, Either[RegistrationError, User]] =
        Eff.send(interpreter.register(email, password))

      override def authn(email: @@[String, EmailAddress], password: String):
          Eff[R, Either[AuthnError, User]] =
        Eff.send(interpreter.authn(email, password))

    }

  type _userRepositoryState[R] = UserRepositoryState |= R

  implicit def effStateInterpreter[R : _userRepositoryState]: Dsl[Eff[R, ?]] =
    effInterpreter[R, UserRepositoryState](stateInterpreter)

}