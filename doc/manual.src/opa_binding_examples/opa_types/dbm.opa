/**
 * Simple page using dbm
**/

import stdlib.system

type dbm = external

type person = {
  string name,
  int age,
  string email
}

/**
 * We open the database when the server is initializing,
 * and close it at the end of the execution, e.g. after a SIGINT
**/
dbm dbm =
  name = "people.dbm"
  dbm =
    match (%%dbm.open_db%%(name)) {
    case { some: dbm }: dbm
    case { none }: @fail("cannot open dbm database at {name}")
    }
  _ = System.at_exit(function () { %%dbm.close_db%%(dbm) })
  dbm


/**
 * Client interface to the Db
**/
exposed
function find_person(name) {
  %%dbm.find_person%%(dbm, name);
}

exposed
function add_person(person) {
  // Because this function is exposed, it should check that person is well formed.
  // e.g. name is not too long ...
  %%dbm.add_person%%(dbm, person);
}



/**
 * Find an entry in the ndbm from the contains of the Name box
**/
function action_find() {
  Dom.clear_value(#age);
  Dom.clear_value(#email);
  name = Dom.get_value(#name)
  match (find_person(name)) {
  case { some: person }:
    Dom.set_value(#age, string_of_int(person.age));
    Dom.set_value(#email, person.email);
    void
  case { none }:
    result("\"{name}\" Not-found")
  }
}

function result(string) {
  #result = string
}

/**
 * Submit the truplet in the ndbm
**/
function action_submit() {
  name = Dom.get_value(#name)
  age = Parser.int(Dom.get_value(#age)) ? 0
  email = Dom.get_value(#email)
  person = ~{ name, age, email }
  add_person(person);
  Dom.clear_value(#name);
  Dom.clear_value(#age);
  Dom.clear_value(#email);
  result("Done");
  void
}

/**
 * Guess the age of the captain
**/

the_captain = "The Captain"
the_question = "What is the age of \"{the_captain}\" ?"

function action_captain() {
  match (find_person(the_captain)) {
  case { some: person }:
    result("The captain is {person.age} years old")
  case { none }:
    result("I don't know")
  }
}

function page() {
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
  <a class="button" ref="#" onclick={function (_) { action_find() }}>Find</a>
  <a class="button" ref="#" onclick={function (_) { action_submit() }}>Submit</a>
  <br/>
  <a class="button" ref="#" onclick={function (_) { action_captain() }}>{the_question}</a>
  <br/>
  <div id="result" />
  </>
}

Server.start(Server.http, {title: "Dbm Binding", ~page})
