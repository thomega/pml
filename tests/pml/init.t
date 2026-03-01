Trivial test (don't use ./pml, because it will overide the environment):
  $ MUSICBRAINZ_CACHE=test-cache2
  $ export MUSICBRAINZ_CACHE
  $ pml init
  pml: initialized cache at 'test-cache2'
  $ pml xxx cache -A
  $ find test-cache2
  test-cache2
  test-cache2/artist
  test-cache2/release
  test-cache2/discid

The same with the command line
  $ ./pml init --cache test-cache3
  pml: initialized cache at 'test-cache3'
  $ ./pml xxx cache -A --cache test-cache3
  $ find test-cache3
  test-cache3
  test-cache3/artist
  test-cache3/release
  test-cache3/discid

Will produce an error:
  $ ./pml xxx cache -A --cache no/such/cache
  pml: no such file or directory: no/such/cache
  [123]
