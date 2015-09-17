Stripe API client for OCaml
===========================

For now, this is just a copy of the files extracted from Esper's
private codebase and because of this it won't build as is.

This implementation uses atdgen, lwt, and cohttp. lwt and cohttp
could possibly be replaced by other libraries via functor
application.

Other private utilities are also involved (logging, timestamps) and
would have somehow to be provided.
