No such domain:
  $ ./run pml admin curl https://foo.bar
  pml: curl 'https://foo.bar': disabled by PML_ONLY_LOCAL_QUERIES environment
  [123]

No such protocoll:
  $ ./run pml admin curl httpx://foo.bar
  pml: curl 'httpx://foo.bar': disabled by PML_ONLY_LOCAL_QUERIES environment
  [123]
