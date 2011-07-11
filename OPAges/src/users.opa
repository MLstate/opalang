package opages
/**
 * {1 About this module}
 * Manage OPAges users. For now OPAges have only one administrator
 * user, that name is "admin".
 */

/**
 * A database stringmap that contains binding beetween administrators
 * name and password.
 */
db /users/admin/pass : stringmap(string)

type User.tokken = option((string,string))

type User.credential = {admin:string} / {anon}

User = {{
  /**
   * (Re)Initialize admin password and print it on server output.
   */
  init_admin() =
    pwd = Random.string(30)
    do /users/admin/pass["admin"] <- pwd
    do jlog("Admin password is \"{pwd}\"")
    void

  /**
   * [is_admin(name, password)] Returns true if [name] is an
   * administrator with a valid [password]
   */
  is_admin(name, password) =
    match ?/users/admin/pass[name] with
    | {none} -> false
    | ~{some} -> some == password

  /**
   * Returns [true] if an admin already exists.
   */
  has_admin() = Option.is_some(?/users/admin/pass["admin"])

}}
