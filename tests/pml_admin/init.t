Trivial test:
  $ MUSICBRAINZ_CACHE=test-cache
  $ export MUSICBRAINZ_CACHE
  $ pml_admin init
  $ pml_admin cache -A
  $ find test-cache
  test-cache
  test-cache/artist
  test-cache/release
  test-cache/discid

The same with the command line
  $ pml_admin init --cache test-cache2
  $ pml_admin cache -A --cache test-cache2
  $ find test-cache2
  test-cache2
  test-cache2/artist
  test-cache2/release
  test-cache2/discid

Will produce an error:
  $ pml_admin cache -A --cache no/such/cache
  pml_admin: no such file or directory: no/such/cache
  [123]
