Invalid discid
  $ ./pml medium fubar
  pml: 'fubar' is not a valid discid
  [123]

Unknown discid
  $ ./pml medium 012345678901234567890123456-
  pml: curl 'https://musicbrainz.org/ws/2/discid/012345678901234567890123456-?fmt=json': disabled by PML_ONLY_LOCAL_QUERIES environment
  [123]

Ambiguous discid
  $ ./pml medium Jo2dahDBN1Q_oT78dplbTyJ08Ig-
  pml: 2 released discs for discid 'Jo2dahDBN1Q_oT78dplbTyJ08Ig-':
         MEDIUM ID/TITLE                      RELEASE ID/TITLE                    
       / ecc0c211-0dd0-36e7-811f-aa8e94889434 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b \
       \ ???                                  Michael Gielen Edition Vol. 6: Co... /
       / cf1cb5dc-8d4b-3526-94a0-5bd368dfee22 04e9ea03-942e-4b09-a2da-3611900cd062 \
       \ ???                                  Ikkan No Yoyo                        /
  [123]

Resolved
  $ ./pml medium Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc0
  Disc:    Jo2dahDBN1Q_oT78dplbTyJ08Ig-
  Medium:  ecc0c211-0dd0-36e7-811f-aa8e94889434
  Release: 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b
  Title:   Michael Gielen Edition Vol. 6: Complete Symphonies
  Artists: Gustav Mahler (comp.) [*1860, †1911]
           Michael Gielen (comp./cond.) [*1927, †2019]
           SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]
  Disc    4:    [ecc0c211-0dd0-36e7-811f-aa8e94889434]
  Track   4.01: Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden
       Artists: Gustav Mahler (comp.) [*1860, †1911]
     Recording: Symphony No. 3 in D minor: Part One: I. Kräftig. Entschieden
  Rec.-Artists: SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]
                Michael Gielen (comp./cond.) [*1927, †2019]

Ripping
  $ ./pml edit Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc0 -AON --edit_prefix '/:.*$//'
  Discid:  Jo2dahDBN1Q_oT78dplbTyJ08Ig-
  Medium:  ecc0c211-0dd0-36e7-811f-aa8e94889434
  Release: 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b
  
  Composer:  'Gustav Mahler (comp.) [*1860, †1911]'
  Performer: 'Michael Gielen (comp./cond.) [*1927, †2019]'
  
  Title:
  
     user choice: 'Sinfonie Nr. 3 d-Moll'
    track prefix: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
         release: 'Michael Gielen Edition Vol. 6: Complete Symphonies'
  
  Directory: 'Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael'
  
  Track  01: 'Erste Abteilung. I. Kräftig. Entschieden'

  $ ./pml rip -d Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc0 --edit_prefix '/:.*$//'
  executing: cdparanoia -w 1 cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav
  executing: mkdir "Mahler, Gustav"
  executing: mkdir "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=01418a7b-1dbe-445c-a40b-eb4f132db8da --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=Jo2dahDBN1Q_oT78dplbTyJ08Ig- --comment TRACKNUMBER=1 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael/01 Erste Abteilung. I. Kräftig. Entschieden.opus"
