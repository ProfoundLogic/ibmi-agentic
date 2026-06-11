     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SA702R3 - API TO CREATE A THREE LINE HEADING FOR POLICY RPTS
     H* *****************************************************************
     H*  Description:                                                   *
     H*
     H*  THIS API WILL CREATE A THREE LINE HEADING THAT WILL INCLUDE
     H*  EMPLOYER NAME, EMP# , CLAIMANT NAME, CLM#, SS# AND DOI.
     H*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*  SEND   :  CO#-GROUP-EMP-DIV
     H*  RECEIVE:  THREE 78 CHAR LONG FIELDS FOR HEADING
     H*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*  THIS IS WHAT THE HEADING WILL LOOK LIKE:
     H*
     H*  Employer: XXXXXXXXXXXXXXXXXXXXXXXXX EMP#: XXX-XXX-XXXXXXXXX-XXX
     H*
     H*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*                                                                 *
     H*  Called From:                                                   *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  *******************************                      *
     H* *****************************************************************
     H** REVISIONS:
     H*
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 - General File Access                                     *
     H*******************************************************************
     FWMEML     IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D HEAD1A          C                   CONST('Employer:')
     D HEAD1B          C                   CONST('Empl#:')
     D HEAD2           C                   CONST('Claimant:')
     D HEAD3A          C                   CONST('Clm#    :')
     D HEAD3B          C                   CONST('Soc Sec#:')
     D HEAD3C          C                   CONST('DOI:')
     D*
     D                 DS
     D  XLHD1                  1     78
     D  XXHD1A                 1      9
     D  XXEMNM                11     35
     D  XXHD1B                37     42
     D  $ECO#                 44     46
     D  $EHYP1                47     47
     D  $EFND#                48     50
     D  $EHYP2                51     51
     D  $EEMP#                52     60
     D  $EHYP3                61     61
     D  $EDIV                 62     64
     D*
     D                 DS
     D  XLHD2                  1     78
     D  XXHD2                  1      9
     D  XXCLNM                11     50
     D*
     D                 DS
     D  XLHD3                  1     78
     D  XXHD3A                 1      9
     D  $CCO#                 11     13
     D  $CHYP1                14     14
     D  $CFND#                15     17
     D  $CHYP2                18     18
     D  $CFYR                 19     21
     D  $CHYP3                22     22
     D  $CCASE                23     29
     D  XXHD3B                37     45
     D  XXSS#                 47     57
     D  XXHD3C                60     63
     D  XXDOI                 65     74
     D*
     D                 DS
     D  SS#                    1     11
     D  SS1                    1      3
     D  DASH1                  4      4
     D  SS2                    5      6
     D  DASH2                  7      7
     D  SS3                    8     11
     D*
     D                 DS
     D  SOCSEC                 1      9
     D  SOCSC1                 1      3
     D  SOCSC2                 4      5
     D  SOCSC3                 6      9
     D*
     D                 DS
     D  ACDT                   1     10
     D  MONTH                  1      2
     D  SLASH1                 3      3
     D  DAY                    4      5
     D  SLASH2                 6      6
     D  YEAR                   7     10
     D*
     D                 DS
     D  XXACDT                 1      8
     D  XXMNTH                 1      2
     D  XXDAY                  3      4
     D  XXYEAR                 5      8

     I*

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@L1             78
     C                   PARM                    @@L2             78
     C                   PARM                    @@L3             78
     C*
     C                   MOVEL     @@CO#         $CCO#
     C                   MOVEL     @@FND         $CFND#
     C                   MOVEL     @@EMP#        $CEMP#            9
     C                   MOVEL     @@DIV         $CDIV             5
     C                   MOVE      @@EMP#        $$EMP#            9 0
     C                   MOVE      @@DIV         $$DIV             5 0
     C                   MOVEL     @@CO#         $ECO#
     C                   MOVEL     @@FND         $EFND#
     C                   MOVEL     '-'           $CHYP1
     C                   MOVEL     '-'           $CHYP2
     C                   MOVEL     '-'           $CHYP3
     C                   MOVEL     '-'           $EHYP1
     C                   MOVEL     '-'           $EHYP2
     C                   MOVEL     '-'           $EHYP3
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*  Change numeric field to character field.
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*
     C**  KEY  LIST FOR CASE MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C**  KEY  LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@EMP#        KKEMP#
     C                   MOVE      @@DIV         KKDIV
     C                   MOVE      ' '           @@RTCD
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* LOAD HEADER INFO
     C*
     C                   MOVEL     HEAD1A        XXHD1A
     C                   MOVEL     HEAD1B        XXHD1B
     C                   MOVEL     HEAD2         XXHD2
     C                   MOVEL     HEAD3A        XXHD3A
     C                   MOVEL     HEAD3B        XXHD3B
     C                   MOVEL     HEAD3C        XXHD3C
     C                   MOVEL     '-'           XXHYP1            1
     C                   MOVEL     '-'           DASH1
     C                   MOVEL     '-'           DASH2
     C                   MOVEL     '/'           SLASH1
     C                   MOVEL     '/'           SLASH2
     C*
     C* GET FIRST HEADER LINE
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   MOVE      EMCO#         #1CO#             3
     C                   MOVE      EMFND         #1FND             3
     C*
     C*
     C                   MOVE      EMFND         #1FND
     C                   MOVE      #1CO#         ##CO#
     C                   MOVE      #1FND         ##WCF#
     C*
     C                   CALL      'WA113R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##WCF#            3 0
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM      *BLANKS       ##RSK#           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C*
     C                   MOVEL     EMEMP#        $EEMP#
     C                   MOVEL     EMDIV         $EDIV
     C*
     C                   MOVEL(P)  EMNAME        XXEMNM
     C                   MOVEL(P)  ##RSK#        XXRSK#           15
     C*
     C                   MOVEL(P)  ##EXT         ##NUM
     C                   CALL      'SA020R'      CNUMPL
     C                   MOVEL     ##NUM         XXEXT             3
     C*
     C                   MOVEL(P)  XLHD1         @@L1
     C                   MOVEL(P)  XLHD2         @@L2
     C                   MOVEL(P)  XLHD3         @@L3
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           @@RTCD
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
