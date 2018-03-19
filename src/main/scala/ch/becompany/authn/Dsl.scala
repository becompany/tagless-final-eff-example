package ch.becompany.authn

import cats.data.State
import cats.implicits._
import ch.becompany.authn.domain.{AuthnError, RegistrationError}
import ch.becompany.shapelessext._
import ch.becompany.shared.domain.{EmailAddress, User}
import org.atnos.eff.{Eff, StateEffect, |=}
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

  type _userRepositoryState[R] = UserRepositoryState |= R

  implicit def stateEffInterpreter[R : _userRepositoryState]: Dsl[Eff[R, ?]] =
    new Dsl[Eff[R, ?]] {

      override def register(email: @@[String, EmailAddress], password: String):
          Eff[R, Either[RegistrationError, User]] =
        for {
          users <- StateEffect.get[R, UserRepository]
          result <- Eff.send((
            if (users.exists(_.email === email))
              State.pure(RegistrationError("User already exists").asLeft)
            else {
              val user = User(email, password)
              State((users: UserRepository) => (users :+ user, user.asRight))
            }): UserRepositoryState[Either[RegistrationError, User]])
        } yield result

      override def authn(email: @@[String, EmailAddress], password: String):
          Eff[R, Either[AuthnError, User]] =
        StateEffect.gets((_: UserRepository)
          .find(user => user.email === email && user.password === password)
          .toRight(AuthnError("Authentication failed")))
    }

}