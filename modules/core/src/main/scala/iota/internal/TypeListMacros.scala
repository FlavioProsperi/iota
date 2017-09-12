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
package internal

import scala.reflect.macros.whitebox.Context

private[iota] final class TypeListMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def materializeTListPos[L <: TList, A](
    implicit
      evL: c.WeakTypeTag[L],
      evA: c.WeakTypeTag[A]
  ): c.Expr[TList.Pos[L, A]] = {

    val L = evL.tpe
    val A = evA.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= A))
                    .filterOrElse(_ >= 0, s"$A is not a member of $L")
    } yield
      q"new _root_.iota.TList.Pos[$L, $A]{ override val index: Int = $index }", true)
  }

  def materializeTListKPos[L <: TListK, F[_]](
    implicit
      evL: c.WeakTypeTag[L],
      evF: c.WeakTypeTag[F[_]]
  ): c.Expr[TListK.Pos[L, F]] = {

    val L = evL.tpe
    val F = evF.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListKTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= F))
                    .filterOrElse(_ >= 0, s"$F is not a member of $L")
    } yield
      q"new _root_.iota.TListK.Pos[$L, $F]{ override val index: Int = $index }", true)
  }

  def materializeTListHPos[L <: TListH, F[_[_]]](
    implicit
      evL: c.WeakTypeTag[L],
      evF: c.WeakTypeTag[F[z] forSome { type z[_] }]
  ): c.Expr[TListH.Pos[L, F]] = {

    val L = evL.tpe
    val F = evF.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListHTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= F))
      .filterOrElse(_ >= 0, s"$F is not a member of $L")
    } yield
      q"new _root_.iota.TListH.Pos[$L, $F]{ override val index: Int = $index }", true)
  }

  def materializeTListCompute[L <: TList, O <: TList](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[TList.Compute.Aux[L, O]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L)
      tpe       = tb.buildTList(algebras)
    } yield
      q"new _root_.iota.TList.Compute[$L]{ override type Out = $tpe }", true)
  }

  def materializeTListKCompute[L <: TListK, O <: TListK](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[TListK.Compute.Aux[L, O]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListKTypes(L)
      tpe       = tb.buildTListK(algebras)
    } yield
      q"new _root_.iota.TListK.Compute[$L]{ override type Out = $tpe }", true)
  }

  def materializeTListHCompute[L <: TListH, O <: TListH](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[TListH.Compute.Aux[L, O]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListHTypes(L)
      tpe       = tb.buildTListH(algebras)
    } yield
      q"new _root_.iota.TListH.Compute[$L]{ override type Out = $tpe }", true)
  }

}
