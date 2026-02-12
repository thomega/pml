Invalid discid
  $ ./run pml medium fubar
  pml: 'fubar' is not a valid discid
  [123]

Unknown discid
  $ ./run pml medium 012345678901234567890123456-
  pml: curl 'https://musicbrainz.org/ws/2/discid/012345678901234567890123456-?fmt=json': disabled by PML_ONLY_LOCAL_QUERIES environment
  [123]

