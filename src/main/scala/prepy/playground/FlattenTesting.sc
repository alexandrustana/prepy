import shapeless.PolyDefns.Case
import shapeless.labelled.{FieldType, KeyTag}
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.tag.Tagged
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, _}
import shapeless.poly.identity

trait TestFlatten[T] {}

object TestFlatten {

  def apply[E](implicit inst: TestFlatten[E]): TestFlatten[E] = inst

  trait primitive extends Poly1 {
    implicit def primitive[K <: Symbol, V](
      implicit
      witness: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], K :: HNil] = {
      println("Primitive type" + witness.value)
      at[FieldType[K, V]](t => witness.value :: HNil)
    }
  }

  object test extends primitive {
    implicit def complex[K <: Symbol, V <: Product, GenV <: HList, FlatV <: HList](
      implicit
      witness: Witness.Aux[K],
      gen:     LabelledGeneric.Aux[V, GenV],
      flatten: FlatMapper.Aux[test.type, GenV, FlatV]
    ): Case.Aux[FieldType[K, V], FlatV] = {
      println("Complex type" + witness.value)
      at[FieldType[K, V]](t => flatten(gen.to(t)))
    }
  }

  implicit def flatten[T, GenT <: HList, KList <: HList, VList <: HList](
    implicit gen: LabelledGeneric.Aux[T, GenT],
    keys:         Keys.Aux[GenT, KList]
  ): TestFlatten[T] = {
    println(keys())
    new TestFlatten[T] {}
  }

}

import TestFlatten._

case class EvenInner(n: Char)
case class Inner(k: String, m: EvenInner)
case class Test(i: Int, j: Boolean, l: Inner)
FlatMapper[test.type ,gen.Repr]
//
//Table[Test]
  val gen = LabelledGeneric[Test]
val keys = Keys[gen.Repr]
val mapped = Mapper[test.type, gen.Repr]
//
//flattener(gen)
//  val test = gen.to(Test(1, true, Inner("a", 'b')))
//val values = Values[gen.Repr]
//FlatMapper[flattener.type, values.Out]

//import TestFlatten._
//TestFlatten[Test]