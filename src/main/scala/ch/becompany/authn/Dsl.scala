package ch.becompany.authn

import cats.Eval
import cats.data.{State, StateT}
import cats.implicits._
import cats.syntax._
import ch.becompany.authn.domain.{AuthnError, RegistrationError}
import ch.becompany.shared.domain.{EmailAddress, User}
import ch.becompany.shapelessext._
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

  type UserRepositoryS[A] = State[UserRepository, A]

  implicit lazy val StateInterpreter: Dsl[UserRepositoryS] = new Dsl[UserRepositoryS] {

    override def register(email: @@[String, EmailAddress], password: String):
        UserRepositoryS[Either[RegistrationError, User]] =
      for {
        users <- State.get[UserRepository]
        result <- (
          if (users.exists(_.email === email))
            State.pure(RegistrationError("User already exists").asLeft)
          else {
            val user = User(email, password)
            State((users: UserRepository) => (users :+ user, user.asRight))
          }): UserRepositoryS[Either[RegistrationError, User]]
      } yield result

    override def authn(email: @@[String, EmailAddress], password: String):
        UserRepositoryS[Either[AuthnError, User]] =
      State.inspect(_
        .find(user => user.email === email && user.password === password)
        .toRight(AuthnError("Authentication failed")))
  }

}