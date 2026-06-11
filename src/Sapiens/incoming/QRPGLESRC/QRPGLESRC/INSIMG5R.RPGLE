     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FSMOCP     UF A E             DISK
      *

     D SUBDIR          C                   CONST('SUBDIREC.001')
     D DCID            C                   CONST('DLS00000.001')
     D                 DS
     D  OCDCID                 1     12
     D  ##DLS                  1      3
     D  ##DC#                  4      8  0
     D  ##PRID                 9      9
     D  ##SD#                 10     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
      *
     C     1             CHAIN     SMOCP                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C*
     C*
     C*
     C                   MOVE      0010101       OCLSEL
     C                   MOVEL     'Y'           OCSELF
     C                   MOVE      0010101       OCLCPY
     C                   MOVEL     'Y'           OCCPYF
     C                   MOVE      0010101       OCLDEL
     C                   MOVEL     'Y'           OCDELF
     C                   MOVE      0010101       OCPFRM
     C                   MOVE      0010101       OCPTO
     C*
     C                   UPDATE    POC01
     C*
     C                   ELSE
     C*
     C                   MOVEL     'G'           OCOSYS
     C                   MOVEL     'OPTMLB01'    OCLIBN
     C                   MOVEL     'OFDG0001'    OCOFLD
     C                   MOVEL     SUBDIR        OCOSBD
     C                   MOVEL     DCID          OCDCID
     C                   MOVE      *ZERO         OC#STG
     C                   MOVEL     'OG0001'      OCVOLI
     C                   MOVE      05000         OCM#DC
     C                   MOVE      010           OCM#SD
     C                   MOVE      00001         OCC#DC
     C                   MOVE      001           OCC#SD
     C                   MOVEL     'A'           OCSTAT
     C                   MOVEL     Q1USER        OCUSER
     C                   MOVEL     Q1PGM         OCPGM
     C                   MOVE      *ZEROS        OCTIME
     C                   MOVE      *ZEROS        OCDT
     C                   MOVEL     Q1USER        OCIUSR
     C                   MOVE      *ZEROS        OCIDT
     C                   MOVEL     'N'           OCPPFL
     C*
     C                   MOVE      0010101       OCLSEL
     C                   MOVEL     'Y'           OCSELF
     C                   MOVE      0010101       OCLCPY
     C                   MOVEL     'Y'           OCCPYF
     C                   MOVE      0010101       OCLDEL
     C                   MOVEL     'Y'           OCDELF
     C                   MOVE      0010101       OCPFRM
     C                   MOVE      0010101       OCPTO
     C*
     C                   WRITE     POC01
     C                   END
      *
     C                   MOVE      *ON           *INLR
