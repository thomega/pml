#! /bin/sh
########################################################################

MUSICBRAINZ_CACHE=test-cache
export MUSICBRAINZ_CACHE

PML_ONLY_LOCAL_QUERIES=true
export PML_ONLY_LOCAL_QUERIES

exec pml "$@"
