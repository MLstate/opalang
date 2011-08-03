/**
 * Simple page using dbm
**/

import stdlib.system

type dbm = external

type person = {
  name : string ;
  age : int ;
  email : string ;
}

/**
 * We open the database when the server is initializing,
 * and close it at the end of the execution, e.g. after a SIGINT
**/
dbm : dbm =
  name = "people.dbm"
  dbm =
    match %%dbm.open_db%%(name) with
    | { some = dbm } -> dbm
    | { none } -> @fail("cannot open dbm database at {name}")
  do System.at_exit(-> %%dbm.close_db%%(dbm))
  dbm

result(string) = Dom.transform([ #result <- string ])

/**
 * Find an entry in the ndbm from the contains of the Name box
**/
@publish action_find() =
  do Dom.clear_value(#age)
  do Dom.clear_value(#email)
  name = Dom.get_value(#name)
  match %%dbm.find_person%%(dbm, name) with
  | { some = person } ->
    do Dom.set_value(#age, string_of_int(person.age))
    do Dom.set_value(#email, person.email)
    void
  | { none } -> result("\"{name}\" Not-found")

/**
 * Submit the truplet in the ndbm
**/
@publish action_submit() =
  name = Dom.get_value(#name)
  age = Parser.int(Dom.get_value(#age)) ? 0
  email = Dom.get_value(#email)
  person = ~{ name age email }
  do %%dbm.add_person%%(dbm, person)
  do Dom.clear_value(#name)
  do Dom.clear_value(#age)
  do Dom.clear_value(#email)
  do result("Done")
  void

/**
 * Guess the age of the captain
**/

the_captain = "The Captain"
the_question = "What is the age of \"{the_captain}\" ?"

@publish action_captain() =
  match %%dbm.find_person%%(dbm, the_captain) with
  | { some = person } ->
    result("The captain is {person.age} years old")
  | { none } ->
    result("I don't know")

page() =
  <>
  <h1>Dbm Binding</h1>
  <h2>New entry</h2>
  <table border="1">
  <tr>
    <th>Name</th>
    <th>Age</th>
    <th>Email</th>
  </tr>
  <tr>
    <td><input id="name"/></td>
    <td><input id="age"/></td>
    <td><input id="email"/></td>
  </tr>
  </table>
  <a class="button" ref="#" onclick={_->action_find()}>Find</a>
  <a class="button" ref="#" onclick={_->action_submit()}>Submit</a>
  <br/>
  <a class="button" ref="#" onclick={_->action_captain()}>{the_question}</a>
  <br/>
  <div id="result" />
  </>

server = one_page_server("Dbm Binding", page)
