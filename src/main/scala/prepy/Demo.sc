import prepy.syntax._
import prepy.formatter.postgresql._

case class UserTable(id: Int, firstName: String, lastName: String, age: Int, address: String)
case class UserName(id: Int, firstName: String, lastName: String)
case class UserAddress(id: Int, address: String)

update[UserTable].set[UserName].apply()