     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H/TITLE CVTWMAHP
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Deactivate Duplicate records in WMAHP                               *
    H*                                                                                    *
    H*  Created: 11/07/14   Programmer C:280   Support: 87172
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
    F*
     FWMAHL     UF   E           K DISK
    F*
    F* Agent History File by CO#/FND/EMP#/DIV/FYR/EFDT/AGT#
    F*
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOF)
    F*
    F* Error List
    F*
     DTIME             s              6s 0
     DDATE             s              7s 0
     Dcount            s              5s 0
    F*
     D                 DS
     D DATAIN                  1    139
     D data1                   1     35
     D AHCO#                   1      2P 0
     D AHFND                   3      4P 0
     D AHEMP#                  5      9P 0
     D AHDIV                  10     12P 0
     D AHFYR                  13     14P 0
     D AHEFDT                 15     18P 0
     D AHAGT#                 19     21P 0
     D AHRT#                  22     23P 0
     D AHPROD                 24     26P 0
     D AHSAGT                 27     29P 0
     D AHSRT#                 30     31P 0
     D AHSPRD                 32     34P 0
     D AHSTAT                 35     35
     D AHUSER                 36     45
     D AHPGM                  46     55
     D AHTIME                 56     61  0
     D AHDT                   62     65P 0
     D AHIUSR                 66     75
     D AHIPGM                 76     85
     D AHITM                  86     91  0
     D AHIDT                  92     95P 0
     D AHAGRC                 96     98P 0
     D AHRTDT                 99    102P 0
     D AHPRRC                103    105P 0
     D AHRTRC                106    107P 0
     D AHEFRC                108    111P 0
     D AH2ARC                112    114P 0
     D AH2PRC                115    117P 0
     D AH2RRC                118    119P 0
     D AHCAF1                120    120
     D AHCAF2                121    121
     D AHCRF1                122    122
     D AHCRF2                123    123
     D AH3AGT                124    126P 0
     D AH3RT#                127    128P 0
     D AH3PRD                129    131P 0
     D AH3ARC                132    134P 0
     D AH3RRC                135    136P 0
     D AH3PRC                137    139P 0
    D*
     D                 DS
     D DATASV                  1    139
     D data2                   1     35
     D  HCO#                   1      2P 0
     D  HFND                   3      4P 0
     D  HEMP#                  5      9P 0
     D  HDIV                  10     12P 0
     D  HFYR                  13     14P 0
     D  HEFDT                 15     18P 0
     D  HAGT#                 19     21P 0
     D  HRT#                  22     23P 0
     D  HPROD                 24     26P 0
     D  HSAGT                 27     29P 0
     D  HSRT#                 30     31P 0
     D  HSPRD                 32     34P 0
     D  HSTAT                 35     35
     D  HUSER                 36     45
     D  HPGM                  46     55
     D  HTIME                 56     61  0
     D  HDT                   62     65P 0
     D  HIUSR                 66     75
     D  HIPGM                 76     85
     D  HITM                  86     91  0
     D  HIDT                  92     95P 0
     D  HAGRC                 96     98P 0
     D  HRTDT                 99    102P 0
     D  HPRRC                103    105P 0
     D  HRTRC                106    107P 0
     D  HEFRC                108    111P 0
     D  H2ARC                112    114P 0
     D  H2PRC                115    117P 0
     D  H2RRC                118    119P 0
     D  HCAF1                120    120
     D  HCAF2                121    121
     D  HCRF1                122    122
     D  HCRF2                123    123
     D  H3AGT                124    126P 0
     D  H3RT#                127    128P 0
     D  H3PRD                129    131P 0
     D  H3ARC                132    134P 0
     D  H3RRC                135    136P 0
     D  H3PRC                137    139P 0
    D*
     D psds          esds                  extname(szq1p)
    D*
    D*================================================================
    D*  User defined fields
    D*================================================================
    C*
    C*
    C*================================================================
    C*  Key List
    C*================================================================
    C*
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *ENTRY        PLIST
     C                   PARM                    UPD               1
    C*
    C*================================================================
     C*
    C*----------------------------------------------------------------------
    C*  Process Request
    C*----------------------------------------------------------------------
    C*
    C*** DO WHILE NOT END OF FILE
    C*
      /free

      // Read Data from WMAHL

      // Date & Time

         time  = %Dec(%Time(): *ISO);
         date  = %dec(%date():*CYMD);
         *inof = *on;

         Read  WMAHL;

b01      Dow Not %EOF(WMAHL);

b02        if ahfnd = 14
             or ahfnd = 18
             or ahfnd = 19
             or ahfnd = 20
             or ahfnd = 21
             or ahfnd = 30;

      // First Time

b03        If data2 = *blanks;
             data2 = data1;
e03        Else;

      // Duplicate ?

b04          if data1 = data2;

      // Print report

b05            if *inof;
                 except head;
                 *inof = *off;
e05            endif;

               count = count + 1;
               ahstat = 'I';

               except detail;

      // Update Record

b05            if upd = 'Y';
                 Update PAH01;
e05            Endif;

x04            else;
                 data2 = data1;
e04            Endif;

e03          Endif;

e02        Endif;

      // Read Next Record

         Read  WMAHL;

e01      Enddo;

         except total;

         *inlr = *on;

      /end-free

    C*
    O*=========================================================================
    O*
     OQSYSPRT   E            HEAD           1 03
     O                                            5 'Time:'
     O                       TIME                14 ' 0:  :  :  '
     O                                           52 'Duplicates in WMAHP'
     O                                           71 'Date:'
     O                       DATE                80 ' 0/  /    '
    O*
     O          E            HEAD           1
     O                                            4 'Pgm:'
     O                       Q1PGM               16
     O                                           71 'PAGE'
     O                       PAGE          Z     78
    O*
     O          E            HEAD           1
     O                                            4 'CO#'
     O                                            9 'GRP'
     O                                           17 'EMP#'
     O                                           26 'DIV'
     O                                           33 'YEAR'
     O                                           44 'EFFECTIVE'
     O                                           53 'AGENT #'
     O                                           61 'RATE#'
     O                                           70 'AGENT #'
     O                                           78 'STATUS'
    O*
     O          E            HEAD           2
     O                                           41 'DATE'
     O                                           54 'ASSIGNED'
    O*
     O          E            Detail         1
     O                       HCO#          Z      4
     O                       HFND          Z      8
     O                       HEMP#         Z     20
     O                       HDIV          Z     27
     O                       HFYR          Z     32
     O                       HEFDT         Y     43
     O                       HAGT#         Z     52
     O                       HRT#          Z     60
     O                       HPROD         Z     68
     O                       HSTAT               75
    O*
     O          E            Detail         2
     O                       AHCO#         Z      4
     O                       AHFND         Z      8
     O                       AHEMP#        Z     20
     O                       AHDIV         Z     27
     O                       AHFYR         Z     32
     O                       AHEFDT        Y     43
     O                       AHAGT#        Z     52
     O                       AHRT#         Z     60
     O                       AHPROD        Z     68
     O                       AHSTAT              75
    O*
     O          E            total       1  1
     O                                           10 'Count'
     O                       count         Z     17
