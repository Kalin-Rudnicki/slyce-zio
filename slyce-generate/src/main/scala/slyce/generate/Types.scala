package slyce.generate

import cats.data.EitherNel

import slyce.core.Marked

type Validated[T] = EitherNel[Marked[String], T]
