import shapeless.PolyDefns.Case
import shapeless.labelled.{FieldType, KeyTag}
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.tag.Tagged
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, _}

trait TestFlatten[T] {}

object TestFlatten {

  def apply[E](implicit inst: TestFlatten[E]): TestFlatten[E] = inst

  trait primitive extends Poly1 {
    implicit def primitive[K <: Symbol, V](
      implicit
      witness: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], V] = {
      println("Primitive type" + witness.value)
      at[FieldType[K, V]](t => t)
    }
  }

  object test extends primitive {
    implicit def complex[K <: Symbol, V <: Product](
      implicit
      witness: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], V] = {
      println("Complex type" + witness.value)
      at[FieldType[K, V]](t => t)
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

case class Inner(k: String, m: Char) extends Product
case class Test(i: Int, j: Boolean, l: Inner)
//
//Table[Test]
  val gen = LabelledGeneric[Test]
val keys = Keys[gen.Repr]
Mapper[test.type, gen.Repr]
//
//flattener(gen)
//  val test = gen.to(Test(1, true, Inner("a", 'b')))
//val values = Values[gen.Repr]
//FlatMapper[flattener.type, values.Out]

//import TestFlatten._
//TestFlatten[Test]