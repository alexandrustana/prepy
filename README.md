# prepy

A small library for generating prepared sql statements based on case classes

## Module

### `prepy.syntax`

Generates string sql queries based on the structure of case classes, this minimizes the effort of manually writing the name of the columns which must be extracted/updated/inserted in a query.

  *Features*:

 1. quick transformation to sql. e.g. `select[A].from[A].apply()`

 2. avoid writing faulty queries. e.g. `update[A].set[A].where("id == 1").apply()`

### `prepy.syntax.doobie`

Easy way of generating doobie queries without have to manully write out queries. e.g. `insert[A].values[A].update()`


## Get started

Prepy is available on Scala 2.12, 2.13

```scala

 libraryDependencies += "com.github.alexandrustana" %% "prepy" % "0.0.4"

```

## Examples

### Converting case classes


```scala
case class UserTable(id: Int, firstName: String, lastName: String, age: Int, address: String)
case class UserName(id: Int, firstName: String, lastName: String)
case class UserAddress(id: Int, address: String) 
```

Now use the prepy magic to generate some sql queries
```scala
import prepy.syntax._

select[UserName].from[UserTable].where("id = 1").apply()
// res0: cats.data.Validated[String,String] = Valid(SELECT id, firstName, lastName FROM UserTable WHERE (id == 1))

update[UserTable].set[UserAddress].where("id = 1").or("id = 2").apply()
// res1: cats.data.Validated[String,String] = Valid(UPDATE UserTable SET id = ?, address = ? WHERE (id == 1) OR (id == 2))

delete[UserTable].where("id = 3").apply()
// res2: cats.data.Validated[String,String] = Valid(DELETE FROM UserTable WHERE (id = 3))
```
Invalid syntax will generate `Invalid` structures with appropriate messages.
```scala
import prepy.syntax._

select[UserName].apply()
// res0: cats.data.Validated.Invalid[String] = Invalid(Incomplete SQL query. `select[T]` must be followed by a `from[K]`)
```

Support PostgreSQL automatic formatter
```scala
import prepy.syntax._
import prepy.formatter.postgresql._

update[UserTable].set[UserName].apply()
//res0: cats.data.Validated[String,String] = Valid(UPDATE user_table SET id = ?, first_name = ?, last_name = ?)
```

Doobie seamless integration 
```scala
import prepy.syntax.doobie._

select[UserName].from[UserTable].query()
//res0: doobie.util.query.Query0[UserTable] = doobie.util.query$Query$$anon$3@69d4503e
```

### Other examples can be found in [tests](src/test/scala/prepy/)

## Contributors and participation

prepy is currently maintained by [Alexandru Stana][alexandrustana].

Any form of contribution (issue report, PR, etc) is more than welcome.

The prepy project supports the [Typelevel][typelevel] [code of conduct][typelevel-coc]
and wants all of its channels (Gitter, GitHub, etc.) to be welcoming environments for
everyone.

## License

prepy is licensed under the [Apache License, Version 2.0][apache2]
(the "License"); you may not use this software except in compliance with
the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[alexandrustana]: https://github.com/alexandrustana
[apache2]: http://www.apache.org/licenses/LICENSE-2.0
[typelevel]: http://typelevel.org/
[typelevel-coc]: http://typelevel.org/conduct.html
[shapeless]: http://github.com/milessabin/shapeless
[cats]: http://github.com/typelevel/cats

