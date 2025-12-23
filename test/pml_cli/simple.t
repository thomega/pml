  $ cat >foo.json <<'EOF'
  > {
  >   "offset-count" : 21,
  >   "id" : "tgAmVy0X8_YXo7e1n_cRN1xI6SY-",
  >   "offsets" : [
  >     150,
  >     29995,
  >     40468,
  >     45691,
  >     101126,
  >     113734,
  >     116839,
  >     125801,
  >     150590,
  >     160538,
  >     170155,
  >     178809,
  >     183003,
  >     192651,
  >     201939,
  >     218662,
  >     235290,
  >     244118,
  >     268011,
  >     299727,
  >     329245
  >   ],
  >   "releases" : [
  >     {
  >       "id" : "f8a9a5e3-b47b-4996-9b83-da0716ad8421",
  >       "status-id" : "4e304316-386d-3409-af2e-78857eec5cfe",
  >       "quality" : "normal",
  >       "title" : "On DSCH",
  >       "asin" : "B095LFLLQ4",
  >       "disambiguation" : "",
  >       "media" : [
  >         {
  >           "format-id" : "9712d52a-4509-3d4b-a1a2-67c88c643e31",
  >           "format" : "CD",
  >           "position" : 1,
  >           "track-count" : 24,
  >           "title" : "",
  >           "discs" : [
  >             {
  >               "sectors" : 286412,
  >               "id" : "Ft8aW122M6v3ViiinKGGHwOlQBw-",
  >               "offsets" : [
  >                 150,
  >                 11950,
  >                 23421,
  >                 27445,
  >                 33910,
  >                 42116,
  >                 50492,
  >                 63842,
  >                 85247,
  >                 91880,
  >                 100015,
  >                 108609,
  >                 130957,
  >                 136494,
  >                 146382,
  >                 151811,
  >                 184200,
  >                 193688,
  >                 200917,
  >                 210678,
  >                 235226,
  >                 240108,
  >                 249136,
  >                 269527
  >               ],
  >               "offset-count" : 24
  >             }
  >           ],
  >           "id" : "14ad2e09-01e2-3567-93ff-82adca6f593b"
  >         },
  >         {
  >           "format-id" : "9712d52a-4509-3d4b-a1a2-67c88c643e31",
  >           "format" : "CD",
  >           "position" : 2,
  >           "track-count" : 24,
  >           "title" : "",
  >           "discs" : [
  >             {
  >               "offset-count" : 24,
  >               "sectors" : 369672,
  >               "offsets" : [
  >                 150,
  >                 11228,
  >                 32651,
  >                 51272,
  >                 63607,
  >                 75177,
  >                 82166,
  >                 94060,
  >                 127761,
  >                 135202,
  >                 151717,
  >                 161447,
  >                 175179,
  >                 185344,
  >                 196317,
  >                 214614,
  >                 238049,
  >                 243791,
  >                 255998,
  >                 265039,
  >                 279472,
  >                 290336,
  >                 308521,
  >                 327407
  >               ],
  >               "id" : "RPtU8gSRzvdtsrgo6g_7oN5Gs9Y-"
  >             }
  >           ],
  >           "id" : "baebe77c-4369-32a6-8507-f5b7d6ee8e87"
  >         },
  >         {
  >           "discs" : [
  >             {
  >               "offset-count" : 21,
  >               "sectors" : 384240,
  >               "id" : "tgAmVy0X8_YXo7e1n_cRN1xI6SY-",
  >               "offsets" : [
  >                 150,
  >                 29995,
  >                 40468,
  >                 45691,
  >                 101126,
  >                 113734,
  >                 116839,
  >                 125801,
  >                 150590,
  >                 160538,
  >                 170155,
  >                 178809,
  >                 183003,
  >                 192651,
  >                 201939,
  >                 218662,
  >                 235290,
  >                 244118,
  >                 268011,
  >                 299727,
  >                 329245
  >               ]
  >             }
  >           ],
  >           "title" : "",
  >           "id" : "d897ad3c-c847-38af-9e7c-62d84b41e6f4",
  >           "format-id" : "9712d52a-4509-3d4b-a1a2-67c88c643e31",
  >           "track-count" : 21,
  >           "position" : 3,
  >           "format" : "CD"
  >         }
  >       ],
  >       "country" : null,
  >       "cover-art-archive" : {
  >         "artwork" : true,
  >         "back" : false,
  >         "darkened" : false,
  >         "front" : true,
  >         "count" : 2
  >       },
  >       "text-representation" : {
  >         "script" : "Latn",
  >         "language" : "eng"
  >       },
  >       "status" : "Official",
  >       "date" : "2021-09-10",
  >       "packaging" : null,
  >       "packaging-id" : null,
  >       "release-events" : [
  >         {
  >           "area" : null,
  >           "date" : "2021-09-10"
  >         }
  >       ],
  >       "barcode" : "194398092126"
  >     }
  >   ],
  >   "sectors" : 384240
  > }

  $ pml_cli musicbrainz -f foo.json -s
  offset-count
  id
  offsets
    21*
  releases
    1*
      id
      status-id
      quality
      title
      asin
      disambiguation
      media
        3*
          format-id
          format
          position
          track-count
          title
          discs
            1*
              sectors
              id
              offsets
                24*
              offset-count
          id
      country
      cover-art-archive
        artwork
        back
        darkened
        front
        count
      text-representation
        script
        language
      status
      date
      packaging
      packaging-id
      release-events
        1*
          area
          date
      barcode
  sectors
