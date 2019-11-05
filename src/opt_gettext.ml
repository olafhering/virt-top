module Gettext = Gettext.Program (
  struct
    let textdomain = "virt-top"
    let codeset = None
    let dir = None
    let dependencies = Gettext.init
  end
) (GettextStub.Native)
