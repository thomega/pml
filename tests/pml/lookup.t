Invalid discid
  $ pml medium fubar
  pml: 'fubar' is not a valid discid
  [123]

Unknown discid
  $ MUSICBRAINZ_CACHE=pml-cache; export MUSICBRAINZ_CACHE
  $ PML_ONLY_LOCAL_QUERIES=true; export PML_ONLY_LOCAL_QUERIES
  $ pml_admin init
  $ pml medium 012345678901234567890123456-
  pml: curl 'https://musicbrainz.org/ws/2/discid/012345678901234567890123456-?fmt=json': disabled by PML_ONLY_LOCAL_QUERIES environment
  [123]
