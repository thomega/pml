# PML: Physical Media Library
## Tagging CD Rips with data from MusicBrainz
The focus of this tool is the almost automatic ripping, encoding and tagging of musical works that
are only a subset of tracks on a disc or are spread over more than one disc.
It supports recordings that are identified by composer, title and performer.

# Typical workflow:
1. Explore the choices for directories, files and tags with `pml edit`.
In this example we extract Shostakovich's 12th symphony on tracks 4 to 7
from a CD that contains the 2nd symphony on tracks 1 to 3.  We have to help
`pml` with the title of the work because the common prefix of the tracks
extends to the roman numeral 'I' starting the movements.
```
$ pml edit jBntohWmL3pfOjiSqgwfDWhbmDg- -M b103 -f 4 -t 'Symphony No. 12 in D minor, Op. 112 "1917"'
Discid:  jBntohWmL3pfOjiSqgwfDWhbmDg-
Medium:  b1036a66-8bdb-3951-96c6-59a44f4515ee
Release: 8db74987-390e-4a77-9ac6-e1cec3b961df

Composer:  'Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]'
Performer: 'Mariss Jansons (cond.) [*1943, †2019]'

Title:

   user choice: 'Symphony No. 12 in D minor, Op. 112 "1917"'
  track prefix: 'Symphony No. 12 in D minor, Op. 112 "1917": I'
        medium: 'Symphonies nos. 2, 12'
       release: 'The Complete Symphonies'

  Artists: 'Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]'
           'Mariss Jansons (cond.) [*1943, †2019]'
           'Andreas Röhn (vln.)'
           'Marco Postinghel [*1968]'
           'Stefan Schilling [*1967]'
           'Chor des Bayerischen Rundfunks [*1946]'
           'Symphonieorchester des Bayerischen Rundfunks [*1949]'

Directory: 'Shostakovich, Dmitri Dmitrievich/Symphony No. 12 in D minor, Op. 112 '1917' - Jansons, Mariss'

Track  01: 'I. Revolutionary Petrograd: Moderato - Allegro'
    orig.: 'Symphony No. 12 in D minor, Op. 112 "1917": I. Revolutionary Petrograd: Moderato - Allegro'
     rec.: 'Symphony no. 12 in D minor, op. 112 "1917": I. Revolutionary Petrograd: Moderato - Allegro'
  Artists: 'Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]'
           'Mariss Jansons (cond.) [*1943, †2019]'
           'Symphonieorchester des Bayerischen Rundfunks [*1949]'
Track  02: 'II. Razliv: Allegro - Adagio'
    orig.: 'Symphony No. 12 in D minor, Op. 112 "1917": II. Razliv: Allegro - Adagio'
     rec.: 'Symphony no. 12 in D minor, op. 112 "1917": II. Razliv: Allegro - Adagio'
  Artists: 'Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]'
           'Mariss Jansons (cond.) [*1943, †2019]'
           'Symphonieorchester des Bayerischen Rundfunks [*1949]'
Track  03: 'III. Aurora: L'istesse tempo - Allegro'
    orig.: 'Symphony No. 12 in D minor, Op. 112 "1917": III. Aurora: L'istesse tempo - Allegro'
     rec.: 'Symphony no. 12 in D minor, op. 112 "1917": III. Aurora: L'istesse tempo - Allegro'
  Artists: 'Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]'
           'Mariss Jansons (cond.) [*1943, †2019]'
           'Symphonieorchester des Bayerischen Rundfunks [*1949]'
Track  04: 'IV. The Dawn of Humanity: L'istesso tempo - Allegretto'
    orig.: 'Symphony No. 12 in D minor, Op. 112 "1917": IV. The Dawn of Humanity: L'istesso tempo - Allegretto'
     rec.: 'Symphony no. 12 in D minor, op. 112 "1917": IV. The Dawn of Humanity: L'istesso tempo - Allegretto'
  Artists: 'Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]'
           'Mariss Jansons (cond.) [*1943, †2019]'
           'Symphonieorchester des Bayerischen Rundfunks [*1949]'
```
2. Call `cdparanoia` and `opusenc`, `flac`, `oggenc`, or `lame` with `pml rip`
using the *same* editing options:
```
$ pml rip jBntohWmL3pfOjiSqgwfDWhbmDg- -M b103 -f 4 -t 'Symphony No. 12 in D minor, Op. 112 "1917"' -r -D working
executing: chdir working
executing: cdparanoia -w 4 cd-jBntohWmL3pfOjiSqgwfDWhbmDg-04.wav
executing: cdparanoia -w 5 cd-jBntohWmL3pfOjiSqgwfDWhbmDg-05.wav
executing: cdparanoia -w 6 cd-jBntohWmL3pfOjiSqgwfDWhbmDg-06.wav
executing: cdparanoia -w 7 cd-jBntohWmL3pfOjiSqgwfDWhbmDg-07.wav
executing: mkdir "Shostakovich, Dmitri Dmitrievich"
executing: mkdir "Shostakovich, Dmitri Dmitrievich/Symphony No. 12 in D minor, Op. 112 '1917' - Jansons, Mariss"
executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=a2eed932-b9a2-38a1-877f-45f7740b5293 --comment MUSICBRAINZ_ALBUMID=8db74987-390e-4a77-9ac6-e1cec3b961df --comment MUSICBRAINZ_DISCID=jBntohWmL3pfOjiSqgwfDWhbmDg- --comment TRACKNUMBER=1 --comment "ALBUM=Symphony No. 12 in D minor, Op. 112 "1917"" --comment "TITLE=Symphony No. 12 in D minor, Op. 112 "1917": I. Revolutionary Petrograd: Moderato - Allegro" --comment "ARTIST=Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]" --comment "ARTIST=Mariss Jansons (cond.) [*1943, †2019]" --comment "ARTIST=Symphonieorchester des Bayerischen Rundfunks [*1949]" cd-jBntohWmL3pfOjiSqgwfDWhbmDg-04.wav "Shostakovich, Dmitri Dmitrievich/Symphony No. 12 in D minor, Op. 112 '1917' - Jansons, Mariss/01 I. Revolutionary Petrograd - Moderato - Allegro.opus"
executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=28c1080b-cc9d-337a-90ec-325920d5896e --comment MUSICBRAINZ_ALBUMID=8db74987-390e-4a77-9ac6-e1cec3b961df --comment MUSICBRAINZ_DISCID=jBntohWmL3pfOjiSqgwfDWhbmDg- --comment TRACKNUMBER=2 --comment "ALBUM=Symphony No. 12 in D minor, Op. 112 "1917"" --comment "TITLE=Symphony No. 12 in D minor, Op. 112 "1917": II. Razliv: Allegro - Adagio" --comment "ARTIST=Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]" --comment "ARTIST=Mariss Jansons (cond.) [*1943, †2019]" --comment "ARTIST=Symphonieorchester des Bayerischen Rundfunks [*1949]" cd-jBntohWmL3pfOjiSqgwfDWhbmDg-05.wav "Shostakovich, Dmitri Dmitrievich/Symphony No. 12 in D minor, Op. 112 '1917' - Jansons, Mariss/02 II. Razliv - Allegro - Adagio.opus"
executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=08a3312f-c5d6-32bc-83c0-5c61ddbb96d6 --comment MUSICBRAINZ_ALBUMID=8db74987-390e-4a77-9ac6-e1cec3b961df --comment MUSICBRAINZ_DISCID=jBntohWmL3pfOjiSqgwfDWhbmDg- --comment TRACKNUMBER=3 --comment "ALBUM=Symphony No. 12 in D minor, Op. 112 "1917"" --comment "TITLE=Symphony No. 12 in D minor, Op. 112 "1917": III. Aurora: L'istesse tempo - Allegro" --comment "ARTIST=Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]" --comment "ARTIST=Mariss Jansons (cond.) [*1943, †2019]" --comment "ARTIST=Symphonieorchester des Bayerischen Rundfunks [*1949]" cd-jBntohWmL3pfOjiSqgwfDWhbmDg-06.wav "Shostakovich, Dmitri Dmitrievich/Symphony No. 12 in D minor, Op. 112 '1917' - Jansons, Mariss/03 III. Aurora - L'istesse tempo - Allegro.opus"
executing: opusenc --quiet --bitrate 128 --comment MUSICBRAINZ_TRACKID=0b29bf24-ea13-30ad-90fc-afd7876df928 --comment MUSICBRAINZ_ALBUMID=8db74987-390e-4a77-9ac6-e1cec3b961df --comment MUSICBRAINZ_DISCID=jBntohWmL3pfOjiSqgwfDWhbmDg- --comment TRACKNUMBER=4 --comment "ALBUM=Symphony No. 12 in D minor, Op. 112 "1917"" --comment "TITLE=Symphony No. 12 in D minor, Op. 112 "1917": IV. The Dawn of Humanity: L'istesso tempo - Allegretto" --comment "ARTIST=Дмитрий Дмитриевич Шостакович (comp.) [*1906, †1975]" --comment "ARTIST=Mariss Jansons (cond.) [*1943, †2019]" --comment "ARTIST=Symphonieorchester des Bayerischen Rundfunks [*1949]" cd-jBntohWmL3pfOjiSqgwfDWhbmDg-07.wav "Shostakovich, Dmitri Dmitrievich/Symphony No. 12 in D minor, Op. 112 '1917' - Jansons, Mariss/04 IV. The Dawn of Humanity - L'istesso tempo - Allegretto.opus"
```
