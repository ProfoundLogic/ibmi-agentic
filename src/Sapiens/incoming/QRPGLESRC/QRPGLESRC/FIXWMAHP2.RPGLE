     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H/TITLE FIXWMAHP2
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Generate records in WMAHP for prior year                            *
    H*                                                                                    *
    H*  Created: 11/07/14   Programmer C:280   Support: 87172
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
    F*
     FWMAHL10   UF A E           K DISK
    F*
    F* Agent History File by CO#/FND/EMP#/DIV/FYR/EFDT/AGT#
    F*
     FWMAHL11   If   E           K DISK    rename(pah01:input) prefix(x)
    F*
    F* Agent History File by CO#/FND/EFDT
    F*
     DTIME             s              6s 0
     DDATE             s              7s 0
    D*
     D                 DS
     D DATAIN                  1    139
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
     D svCO#                   1      2P 0
     D svFND                   3      4P 0
     D svEMP#                  5      9P 0
     D svDIV                  10     12P 0
     D svFYR                  13     14P 0
     D svEFDT                 15     18P 0
     D svAGT#                 19     21P 0
     D svRT#                  22     23P 0
     D svPROD                 24     26P 0
     D svSAGT                 27     29P 0
     D svSRT#                 30     31P 0
     D svSPRD                 32     34P 0
     D svSTAT                 35     35
     D svUSER                 36     45
     D svPGM                  46     55
     D svTIME                 56     61  0
     D svDT                   62     65P 0
     D svIUSR                 66     75
     D svIPGM                 76     85
     D svITM                  86     91  0
     D svIDT                  92     95P 0
     D svAGRC                 96     98P 0
     D svRTDT                 99    102P 0
     D svPRRC                103    105P 0
     D svRTRC                106    107P 0
     D svEFRC                108    111P 0
     D sv2ARC                112    114P 0
     D sv2PRC                115    117P 0
     D sv2RRC                118    119P 0
     D svCAF1                120    120
     D svCAF2                121    121
     D svCRF1                122    122
     D svCRF2                123    123
     D sv3AGT                124    126P 0
     D sv3RT#                127    128P 0
     D sv3PRD                129    131P 0
     D sv3ARC                132    134P 0
     D sv3RRC                135    136P 0
     D sv3PRC                137    139P 0
    D*
    D*
     D psds          esds                  extname(szq1p)
    D*
    D*================================================================
    D*  User defined fields
    D*================================================================
    C*
     C     *LIKE         DEFINE    AHEFDT        EFDT
     C     *LIKE         DEFINE    AHFYR         fmFYR
     C     *LIKE         DEFINE    AHFYR         toFYR
     C     *LIKE         DEFINE    AHFYR         FYR
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

      // Date & Time

         time  = %Dec(%Time(): *ISO);
         date  = %dec(%date():*CYMD);

      // Read Data from WMAHL10

         Read  WMAHL10;

b01      Dow Not %EOF(WMAHL10);

b02      if ahfnd = 14
           or ahfnd = 18
           or ahfnd = 19
           or ahfnd = 20
           or ahfnd = 21
           or ahfnd = 30;

b03      if ahstat = 'A';

      // First Time

b04      If datasv = *blanks;
           datasv = datain;
e04      Else;

b05      if svco#  <> ahco#
           or svfnd  <> ahfnd
           or svemp# <> ahemp#
           or svdiv  <> ahdiv
           or svfyr  <> ahfyr
           or svefdt <> ahefdt
           or svagt# <> ahagt#;

      // Find Latest Year

         efdt = *hival;

         setgt  (svco#:svfnd:svdiv:svemp#:efdt) wmahl11;
         readpe (svco#:svfnd:svdiv:svemp#) wmahl11;

b06      if %found(wmahl11);

           tofyr = svfyr;
           fmfyr = xahfyr;

      // Process all years

b07      dou tofyr >= fmfyr;

      // Determine if record already exits

           efdt = 1990102;

         chain  (svco#:svfnd:svemp#:svdiv:tofyr:efdt:xahagt#) wmahl10;

b08      if not %found(wmahl10);

      // Update Record

b09      if upd = 'Y';

b10      if svfnd = 14 or svfnd = 30;
           svrt# = 200;
         else;
b11      if svfnd = 18 or svfnd = 19 or svfnd = 20 or svfnd = 21;
           svrt# = 100;
e11      endif;
e10      endif;

                 AHCO#  = svCO#;
                 AHFND  = svFND;
                 AHEMP# = svEMP#;
                 AHDIV  = svDIV;
                 AHFYR  = tofyr;
                 AHAGT# = XAHAGT#;
                 AHEFDT = efdt;
                 AHRT#  = svRT#;
                 AHPROD = svPROD;
                 AHSAGT = svSAGT;
                 AHSRT# = svSRT#;
                 AHSPRD = svSPRD;
                 AHSTAT = svSTAT;
                 AHUSER = q1user;
                 AHPGM  = q1pgm;
                 ahtime = %Dec(%Time(): *ISO);
                 ahdt   = %dec(%date():*CYMD);
                 AHIUSR = q1user;
                 AHIPGM = q1pgm;
                 ahitm  = %Dec(%Time(): *ISO);
                 ahidt  = %dec(%date():*CYMD);
                 AHAGRC = svAGRC;
                 AHRTDT = svRTDT;
                 AHPRRC = svPRRC;
                 AHRTRC = svRTRC;
                 AHEFRC = svEFRC;
                 AH2ARC = sv2ARC;
                 AH2PRC = sv2PRC;
                 AH2RRC = sv2RRC;
                 AHCAF1 = svCAF1;
                 AHCAF2 = svCAF2;
                 AHCRF1 = svCRF1;
                 AHCRF2 = svCRF2;
                 AH3AGT = sv3AGT;
                 AH3RT# = sv3RT#;
                 AH3PRD = sv3PRD;
                 AH3ARC = sv3ARC;
                 AH3RRC = sv3RRC;
                 AH3PRC = sv3PRC;

                 Write  PAH01;

e09      Endif;

e08      Endif;

           tofyr  = tofyr + 1;

         if tofyr = 999;
           leave;
         endif;

e07      Enddo;

      // reset to last record processed

         setgt (svco#:svfnd:svemp#:svdiv:tofyr:efdt:xahagt#) wmahl10;
         Read  wmahl10;

         datasv = datain;

e06      Endif;

e05      Endif;

e04      Endif;

e03      Endif;

e02      Endif;

         Read  WMAHL10;

e01      Enddo;

         *inlr = *on;

      /end-free

