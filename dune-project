(lang dune 1.11)

(generate_opam_files true)

(license "GPL-2.0-or-later")

(authors "Richard W.M. Jones <rjones@redhat.com>")

(maintainers "Richard W.M. Jones <rjones@redhat.com>")

(homepage "http://people.redhat.com/~rjones/virt-top/")

(bug_reports "https://github.com/olafhering/virt-top/issues")

(source
 (github olafhering/virt-top))

(package
 (name virt-top)
 (synopsis "top-like utility")
 (description
   "virt-top is a top-like utility for showing stats of virtualized domains. Many keys and command line options are the same as for ordinary top. .")
 (depends
  (ocaml
   (>= 4.02.0))
  (dune
   (>= 1.11))
  calendar
  csv
  dune.configurator
  gettext
  xml-light))
