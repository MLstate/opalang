##register init: string, string, string -> void
##args(id, pubkey, theme)
{
  Recaptcha.create(pubkey,
      id,
      {
        theme: theme,
        callback: Recaptcha.focus_response_field
      }
  );
}

##register get_challenge: -> string
##args()
{
    return (Recaptcha.get_challenge()||"")
}

##register get_response: -> string
##args()
{
    return (Recaptcha.get_response()||"")
}

##register destroy: -> void
##args()
{
    Recaptcha.destroy();
}

##register reload: -> void
##args()
{
    Recaptcha.reload();
}
