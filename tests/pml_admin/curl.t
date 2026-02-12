No such domain:
  $ pml_admin curl https://foo.bar
  pml_admin: curl https://foo.bar failed: Could not resolve host: foo.bar
  [123]

No such protocoll:
  $ pml_admin curl httpx://foo.bar
  pml_admin: curl httpx://foo.bar failed: Protocol "httpx" not supported
  [123]
