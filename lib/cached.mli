(* cached.mli -- part of PML (Physical Media Library)

  Copyright (C) 2026 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

(** Query the Musicbrainz database with local caching. *)

(** Use this to avoid excessive network traffic when finetuning
    the tagging of tracks. *)

module type T =
  sig

    val get : root:string -> string -> (string, string) result
    (** Return the JSON for the given key, preferring the cache located at [root].
        If there is no local entry for the key, it will be created from the
        result of the remote lookup. *)

    val local : root:string -> string -> (string option, string) result
    (** Return the JSON for the given key, using only the cache located at [root]. *)

    val remote : string -> (string, string) result
    (** Return the JSON for the given key, ignoring any cache. *)

    val all_local : root:string -> ((string * string) list, string) result
    (** Return the cached [key] JSON pairs. *)

    val url : string -> (string, string) result
    (** Return the URL for querying Musicbrainz for the entry corresponding to a key. *)

    module M : Map.S with type key = string

    val map_of_ids : root:string -> (string -> ('a, string) result) ->
                     string list -> ('a M.t, string) result
    (** Build a map of [(key, value)] pairs using remote lookup, if necessary. *)

    val map_of_ids_local : root:string -> (string -> ('a, string) result) ->
                     string list -> ('a M.t, string) result
    (** Build a map of [(key, value)] pairs that are available locally. *)

    module Internal : Cache.T with type key = string and type value = string
    (** Access the public interface of the [Cache.T] used to implement
        this cached table. *)

  end

val valid_discid : string -> (string, string) result
(** Check that a string is valid discid
    {v
     <element name="disc">
         <attribute name="id">
             <data type="string">
                 <param name="pattern">[a-zA-Z0-9._]{27}-</param>
             </data>
         </attribute>
         ...    
     </element>
     v} *)

module Discid : T
(** Access information about a disc, in particular the MBIDs of the
    releases containing the disc, by the [discid]. *)

(** The
    {{: https://github.com/metabrainz/libmusicbrainz/blob/master/examples/cdlookup_c.c }old example}
    in the MusicBrainz sources suggests that it is impossible to
    get all information from a single [discid] lookup.  This is (no longer?)
    correct: we could access all required information in one step by
    adding [inc=artist-credits+recordings] to the request.   However,
    this results in substational redundancy, whenever a release contains
    many discs, as the information is repeated for every disc in the release.

    Therefore, we continue to use the two step approach suggested by the example. *)

val releases_of_discid : root:string -> string -> (string list, string) result
(** There can be more than one release of a given disc.  Return
    them as a list of MBID strings. *)

val valid_mbid : string -> (string, string) result
(** Check that a string is a MBID (i.e. UUID)
    {v
     <define name="def_uuid">
         <data type="string">
             <param name="pattern">[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}</param>
         </data>
     </define>
     v}
     Note that this does not reject upper case hexadecimals and
     silently changes them to lowercase instead. *)

module Release : T
(** Access more detailled information about a release from its MBID: tracks, artists, etc. *)

module Artist : T
(** Access more detailled information about an artist: life span and aliases.
    {b The lifespan can be used for distinguishing composers from performers!} *)

val init : root:string -> (unit, string) result
(** Initialize the cache beneath directory [root]. *)
