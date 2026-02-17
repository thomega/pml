As first movement of symphony
  $ ./pml edit Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc --edit_prefix '/:.*$//'
  Discid:  Jo2dahDBN1Q_oT78dplbTyJ08Ig-
  Medium:  ecc0c211-0dd0-36e7-811f-aa8e94889434
  Release: 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b
  
  Composer:  'Gustav Mahler (comp.) [*1860, †1911]'
  Performer: 'Michael Gielen (comp./cond.) [*1927, †2019]'
  
  Title:
  
     user choice: 'Sinfonie Nr. 3 d-Moll'
    track prefix: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
         release: 'Michael Gielen Edition Vol. 6: Complete Symphonies'
  
    Artists: 'Gustav Mahler (comp.) [*1860, †1911]'
             'Michael Gielen (comp./cond.) [*1927, †2019]'
             'SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]'
  
  Directory: 'Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael'
  
  Track  01: 'Erste Abteilung. I. Kräftig. Entschieden'
       full: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
       rec.: 'Symphony No. 3 in D minor: Part One: I. Kräftig. Entschieden'
    Artists: 'Gustav Mahler (comp.) [*1860, †1911]'
             'Michael Gielen (comp./cond.) [*1927, †2019]'
             'SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]'

  $ ./pml rip -d Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc --edit_prefix '/:.*$//'
  executing: cdparanoia -w 1 cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav
  executing: mkdir "Mahler, Gustav"
  executing: mkdir "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=01418a7b-1dbe-445c-a40b-eb4f132db8da --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=Jo2dahDBN1Q_oT78dplbTyJ08Ig- --comment TRACKNUMBER=1 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael/01 Erste Abteilung. I. Kräftig. Entschieden.opus"

As first movement of symphony (default), not chopping after colon produces empty track:
  $ ./pml edit Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc
  Discid:  Jo2dahDBN1Q_oT78dplbTyJ08Ig-
  Medium:  ecc0c211-0dd0-36e7-811f-aa8e94889434
  Release: 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b
  
  Composer:  'Gustav Mahler (comp.) [*1860, †1911]'
  Performer: 'Michael Gielen (comp./cond.) [*1927, †2019]'
  
  Title:
  
    track prefix: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
         release: 'Michael Gielen Edition Vol. 6: Complete Symphonies'
  
    Artists: 'Gustav Mahler (comp.) [*1860, †1911]'
             'Michael Gielen (comp./cond.) [*1927, †2019]'
             'SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]'
  
  Directory: 'Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Erste Abteilung. I. Kräftig. Entschieden - Gielen, Michael'
  
  Track  01: ''
       full: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
       rec.: 'Symphony No. 3 in D minor: Part One: I. Kräftig. Entschieden'
    Artists: 'Gustav Mahler (comp.) [*1860, †1911]'
             'Michael Gielen (comp./cond.) [*1927, †2019]'
             'SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]'

  $ ./pml rip -d Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc
  executing: cdparanoia -w 1 cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav
  executing: mkdir "Mahler, Gustav"
  executing: mkdir "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Erste Abteilung. I. Kräftig. Entschieden - Gielen, Michael"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=01418a7b-1dbe-445c-a40b-eb4f132db8da --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=Jo2dahDBN1Q_oT78dplbTyJ08Ig- --comment TRACKNUMBER=1 --comment "ALBUM=Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Erste Abteilung. I. Kräftig. Entschieden - Gielen, Michael/01.opus"

As if single movement (not correct for this piece!)
  $ ./pml edit Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc --edit_prefix '/:.*$//' --single
  Discid:  Jo2dahDBN1Q_oT78dplbTyJ08Ig-
  Medium:  ecc0c211-0dd0-36e7-811f-aa8e94889434
  Release: 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b
  
  Composer:  'Gustav Mahler (comp.) [*1860, †1911]'
  Performer: 'Michael Gielen (comp./cond.) [*1927, †2019]'
  
  Title:
  
     user choice: 'Sinfonie Nr. 3 d-Moll'
    track prefix: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
    track prefix: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
         release: 'Michael Gielen Edition Vol. 6: Complete Symphonies'
  
    Artists: 'Gustav Mahler (comp.) [*1860, †1911]'
             'Michael Gielen (comp./cond.) [*1927, †2019]'
             'SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]'
  
  Directory: 'Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael'
  
      Track: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
       rec.: 'Symphony No. 3 in D minor: Part One: I. Kräftig. Entschieden'
    Artists: 'Gustav Mahler (comp.) [*1860, †1911]'
             'Michael Gielen (comp./cond.) [*1927, †2019]'
             'SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]'

  $ ./pml rip -d Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc --edit_prefix '/:.*$//' --single
  executing: cdparanoia -w 1 cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav
  executing: mkdir "Mahler, Gustav"
  executing: mkdir "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=01418a7b-1dbe-445c-a40b-eb4f132db8da --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=Jo2dahDBN1Q_oT78dplbTyJ08Ig- --comment TRACKNUMBER=1 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael/Sinfonie Nr. 3 d-Moll - Erste Abteilung. I. Kräftig. Entschieden.opus"

As if single movement (not correct for this piece!), not chopping after colon:
  $ ./pml edit Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc --single
  Discid:  Jo2dahDBN1Q_oT78dplbTyJ08Ig-
  Medium:  ecc0c211-0dd0-36e7-811f-aa8e94889434
  Release: 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b
  
  Composer:  'Gustav Mahler (comp.) [*1860, †1911]'
  Performer: 'Michael Gielen (comp./cond.) [*1927, †2019]'
  
  Title:
  
    track prefix: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
    track prefix: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
         release: 'Michael Gielen Edition Vol. 6: Complete Symphonies'
  
    Artists: 'Gustav Mahler (comp.) [*1860, †1911]'
             'Michael Gielen (comp./cond.) [*1927, †2019]'
             'SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]'
  
  Directory: 'Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Erste Abteilung. I. Kräftig. Entschieden - Gielen, Michael'
  
      Track: 'Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden'
       rec.: 'Symphony No. 3 in D minor: Part One: I. Kräftig. Entschieden'
    Artists: 'Gustav Mahler (comp.) [*1860, †1911]'
             'Michael Gielen (comp./cond.) [*1927, †2019]'
             'SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]'

  $ ./pml rip -d Jo2dahDBN1Q_oT78dplbTyJ08Ig- -M ecc --single
  executing: cdparanoia -w 1 cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav
  executing: mkdir "Mahler, Gustav"
  executing: mkdir "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Erste Abteilung. I. Kräftig. Entschieden - Gielen, Michael"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=01418a7b-1dbe-445c-a40b-eb4f132db8da --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=Jo2dahDBN1Q_oT78dplbTyJ08Ig- --comment TRACKNUMBER=1 --comment "ALBUM=Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Erste Abteilung. I. Kräftig. Entschieden" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-Jo2dahDBN1Q_oT78dplbTyJ08Ig-01.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Erste Abteilung. I. Kräftig. Entschieden - Gielen, Michael/Sinfonie Nr. 3 d-Moll - Erste Abteilung. I. Kräftig. Entschieden.opus"
