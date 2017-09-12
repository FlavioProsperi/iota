/*
 * Copyright 2016-2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package iota

/** A heterogenous list of type constructors of shape H: `T[_[_]]` */
trait TListH

object TListH {

  /** A syntactic sugar alias for [[TConsH]] */
  type ::[H[_[_]], T <: TListH] = TConsH[H, T]

  /** A syntactic sugar alias for [[TConsH]] */
  type :::[H[_[_]], T <: TListH] = TConsH[H, T]

  /** A type class that witnesses the position of type constructor `F` in type
    * constructor list `L`
    */
  trait Pos[L <: TListH, F[_[_]]] {
    def index: Int
  }

  object Pos {
    def apply[L <: TListH, F[_[_]]](implicit ev: Pos[L, F]): Pos[L, F] = ev
    implicit def materializePos[L <: TListH, F[_[_]]]: Pos[L, F] =
      macro internal.TypeListMacros.materializeTListHPos[L, F]
  }

  object Op {
    type Concat [L <: TListH, R <: TListH]       <: TListH
    type Reverse[L <: TListH]                    <: TListH
    type Take   [N <: SingletonInt, L <: TListH] <: TListH
    type Drop   [N <: SingletonInt, L <: TListH] <: TListH
    type Remove [K[_[_]], L <: TListH]           <: TListH
  }

  trait Compute[L <: TListH] {
    type Out <: TListH
  }

  object Compute {
    type Aux[L <: TListH, O <: TListH] = Compute[L] { type Out = O }

    def apply[L <: TListH](implicit ev: Compute[L]): Compute.Aux[L, ev.Out] = ev
    implicit def materializeCompute[L <: TListH, O <: TListH]: Aux[L, O] =
      macro internal.TypeListMacros.materializeTListHCompute[L, O]
  }

}
