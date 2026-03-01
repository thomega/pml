No such domain:
  $ ./pml xxx curl https://foo.bar
  pml: curl 'https://foo.bar': disabled by PML_ONLY_LOCAL_QUERIES environment
  [123]

No such protocoll:
  $ ./pml xxx curl httpx://foo.bar
  pml: curl 'httpx://foo.bar': disabled by PML_ONLY_LOCAL_QUERIES environment
  [123]
