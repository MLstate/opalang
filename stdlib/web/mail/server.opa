type SmtpServer.failure = {dont_exist}
                        / {undisponible}
                        / {stopped}
                        / {unfound}
                        / {forbidden}
                        / {error : string}

type SmtpServer.result = { success }
                       / { failure : SmtpServer.failure}

type SmtpServer.handler = string, list(string), string -> SmtpServer.result

SmtpServer= {{
  @private
  init_server = %% BslMail.Mailserver.init_server %%
  start(ip : ip ,port : int, ssl : option(SSL.secure_type), handler : SmtpServer.handler)=
    new_handler(f,t,c) = match handler(f,t,c) with
                         | {success} -> (200,"done!")
                         | {failure = {undisponible}} -> (450,"undone because mail box not disponible")
                         | {failure = {stopped}} -> (451,"treatment error")
                         | {failure = {unfound}} -> (550,"No access to mail box => unfound")
                         | {failure = {dont_exist}} -> (553,"mail box's name is not allowed")
                         | {failure = {forbidden}} -> (553,"mail box's name is not allowed")
                         | {failure = {error = txt}} -> (503,txt)

    st = match ssl with
      | {~some} -> some
      | {none} -> SSL.make_secure_type({none},{none})

    init_server(port, IPv4.string_of_ip(ip), st, new_handler)




}}
