Multiple movements with offset (suppress artists, for brevity)

  $ ./pml edit rYnHujOBunoYhjdNe88TV_aD4A4- -AON -o 1 --edit_prefix '/:.*$//'
  Discid:  rYnHujOBunoYhjdNe88TV_aD4A4-
  Medium:  a2c52ed7-b1d4-32ad-81a6-abb48d469a34
  Release: 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b
  
  Composer:  'Gustav Mahler (comp.) [*1860, †1911]'
  Performer: 'Michael Gielen (comp./cond.) [*1927, †2019]'
  
  Title:
  
    track prefix: 'Sinfonie Nr. 3 d-Moll'
    track prefix: 'Sinfonie Nr. 3 d-Moll: Zweite Abteilung'
         release: 'Michael Gielen Edition Vol. 6: Complete Symphonies'
  
  Directory: 'Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael'
  
  Track  02: 'Zweite Abteilung. II. Tempo di Menuetto. Sehr mäßig'
  Track  03: 'Zweite Abteilung. III. Comodo. Scherzando. Ohne Hast'
  Track  04: 'Zweite Abteilung. IV. Sehr langsam. Misterioso. Durchaus'
  Track  05: 'Zweite Abteilung. V. Lustig im Tempo und keck im Ausdruck'
  Track  06: 'Zweite Abteilung. VI. Langsam. Ruhevoll. Empfunden'

Let it rip:

  $ ./pml rip -d rYnHujOBunoYhjdNe88TV_aD4A4- -o 1 --edit_prefix '/:.*$//'
  executing: cdparanoia -d /dev/cdrom -w 1 cd-rYnHujOBunoYhjdNe88TV_aD4A4-01.wav
  executing: cdparanoia -d /dev/cdrom -w 2 cd-rYnHujOBunoYhjdNe88TV_aD4A4-02.wav
  executing: cdparanoia -d /dev/cdrom -w 3 cd-rYnHujOBunoYhjdNe88TV_aD4A4-03.wav
  executing: cdparanoia -d /dev/cdrom -w 4 cd-rYnHujOBunoYhjdNe88TV_aD4A4-04.wav
  executing: cdparanoia -d /dev/cdrom -w 5 cd-rYnHujOBunoYhjdNe88TV_aD4A4-05.wav
  executing: mkdir "Mahler, Gustav"
  executing: mkdir "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=ff5def33-3202-442e-b767-142a34517285 --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=2 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. II. Tempo di Menuetto. Sehr mäßig" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-01.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael/02 Zweite Abteilung. II. Tempo di Menuetto. Sehr mäßig.opus"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=56d6dbcb-a51f-4cb1-9d61-d0d205b2ba51 --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=3 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. III. Comodo. Scherzando. Ohne Hast" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-02.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael/03 Zweite Abteilung. III. Comodo. Scherzando. Ohne Hast.opus"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=57b8a188-9eb5-40c1-b59d-b887987f5ee5 --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=4 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. IV. Sehr langsam. Misterioso. Durchaus" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=Cornelia Kallisch (S./Mez.) [*1956]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-03.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael/04 Zweite Abteilung. IV. Sehr langsam. Misterioso. Durchaus.opus"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=59a58778-03b5-41b7-8e09-b770dd1a22be --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=5 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. V. Lustig im Tempo und keck im Ausdruck" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=Cornelia Kallisch (S./Mez.) [*1956]" --comment "ARTIST=EuropaChorAkademie [*1997]" --comment "ARTIST=Freiburger Domsingknaben [*1970]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-04.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael/05 Zweite Abteilung. V. Lustig im Tempo und keck im Ausdruck.opus"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=6763425d-96a6-4388-95d3-3706ab115c5d --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=6 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. VI. Langsam. Ruhevoll. Empfunden" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-05.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll - Gielen, Michael/06 Zweite Abteilung. VI. Langsam. Ruhevoll. Empfunden.opus"

The same, but don't include performer in the directory

  $ ./pml edit rYnHujOBunoYhjdNe88TV_aD4A4- -AON -o 1 --edit_prefix '/:.*$//' -u
  Discid:  rYnHujOBunoYhjdNe88TV_aD4A4-
  Medium:  a2c52ed7-b1d4-32ad-81a6-abb48d469a34
  Release: 0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b
  
  Artist:    'Gustav Mahler (comp.) [*1860, †1911]'
  
  Title:
  
    track prefix: 'Sinfonie Nr. 3 d-Moll'
    track prefix: 'Sinfonie Nr. 3 d-Moll: Zweite Abteilung'
         release: 'Michael Gielen Edition Vol. 6: Complete Symphonies'
  
  Directory: 'Mahler, Gustav/Sinfonie Nr. 3 d-Moll'
  
  Track  02: 'Zweite Abteilung. II. Tempo di Menuetto. Sehr mäßig'
  Track  03: 'Zweite Abteilung. III. Comodo. Scherzando. Ohne Hast'
  Track  04: 'Zweite Abteilung. IV. Sehr langsam. Misterioso. Durchaus'
  Track  05: 'Zweite Abteilung. V. Lustig im Tempo und keck im Ausdruck'
  Track  06: 'Zweite Abteilung. VI. Langsam. Ruhevoll. Empfunden'

Let it rip:

  $ ./pml rip -d rYnHujOBunoYhjdNe88TV_aD4A4- -o 1 --edit_prefix '/:.*$//' -u
  executing: cdparanoia -d /dev/cdrom -w 1 cd-rYnHujOBunoYhjdNe88TV_aD4A4-01.wav
  executing: cdparanoia -d /dev/cdrom -w 2 cd-rYnHujOBunoYhjdNe88TV_aD4A4-02.wav
  executing: cdparanoia -d /dev/cdrom -w 3 cd-rYnHujOBunoYhjdNe88TV_aD4A4-03.wav
  executing: cdparanoia -d /dev/cdrom -w 4 cd-rYnHujOBunoYhjdNe88TV_aD4A4-04.wav
  executing: cdparanoia -d /dev/cdrom -w 5 cd-rYnHujOBunoYhjdNe88TV_aD4A4-05.wav
  executing: mkdir "Mahler, Gustav"
  executing: mkdir "Mahler, Gustav/Sinfonie Nr. 3 d-Moll"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=ff5def33-3202-442e-b767-142a34517285 --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=2 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. II. Tempo di Menuetto. Sehr mäßig" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-01.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll/02 Zweite Abteilung. II. Tempo di Menuetto. Sehr mäßig.opus"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=56d6dbcb-a51f-4cb1-9d61-d0d205b2ba51 --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=3 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. III. Comodo. Scherzando. Ohne Hast" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-02.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll/03 Zweite Abteilung. III. Comodo. Scherzando. Ohne Hast.opus"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=57b8a188-9eb5-40c1-b59d-b887987f5ee5 --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=4 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. IV. Sehr langsam. Misterioso. Durchaus" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=Cornelia Kallisch (S./Mez.) [*1956]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-03.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll/04 Zweite Abteilung. IV. Sehr langsam. Misterioso. Durchaus.opus"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=59a58778-03b5-41b7-8e09-b770dd1a22be --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=5 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. V. Lustig im Tempo und keck im Ausdruck" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=Cornelia Kallisch (S./Mez.) [*1956]" --comment "ARTIST=EuropaChorAkademie [*1997]" --comment "ARTIST=Freiburger Domsingknaben [*1970]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-04.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll/05 Zweite Abteilung. V. Lustig im Tempo und keck im Ausdruck.opus"
  executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=6763425d-96a6-4388-95d3-3706ab115c5d --comment MUSICBRAINZ_ALBUMID=0ad2c0d1-0e19-4247-b7c2-a3003f85dc0b --comment MUSICBRAINZ_DISCID=rYnHujOBunoYhjdNe88TV_aD4A4- --comment TRACKNUMBER=6 --comment "ALBUM=Sinfonie Nr. 3 d-Moll" --comment "TITLE=Sinfonie Nr. 3 d-Moll: Zweite Abteilung. VI. Langsam. Ruhevoll. Empfunden" --comment "ARTIST=Gustav Mahler (comp.) [*1860, †1911]" --comment "ARTIST=Michael Gielen (comp./cond.) [*1927, †2019]" --comment "ARTIST=SWR Sinfonieorchester Baden‐Baden und Freiburg [*1946, †2016]" cd-rYnHujOBunoYhjdNe88TV_aD4A4-05.wav "Mahler, Gustav/Sinfonie Nr. 3 d-Moll/06 Zweite Abteilung. VI. Langsam. Ruhevoll. Empfunden.opus"
