     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*******************************************************************
     H/TITLE SA014R - Return Multi Split Value
     H*******************************************************************
     H*  Description:  Checks System and State Flags to determine       *
     H*                if lines should be split or not.                 *
     H*                                                                 *
     H*******************************************************************
     H*  Created on 02/01/2009 by c:62 for Project MC288                *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*
     H*******************************************************************
     FSTABL     IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*
     C*          M A I N     L O G I C
     C*
     C     *ENTRY        PLIST
01   C                   PARM                    @@ST              2            State
02   C                   PARM                    @@BTYP            1            Bill Type
03   C                   PARM                    @@FDAT            7 0          Line Date
04   C                   PARM                    @@SPLT            1            Split Lines
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*  Call API to see if system flag is on.
     C*
     C                   CALL      'SA001R'
1    C                   PARM      *BLANK        ##WCIF            1
2    C                   PARM      *BLANK        ##MCIF            1
3    C                   PARM      *BLANK        ##WCIN            1
4    C                   PARM      *BLANK        ##AR              1
5    C                   PARM      *BLANK        ##NTWK            1
6    C                   PARM      *BLANK        ##UR              1
7    C                   PARM      *BLANK        ##MED             1
8    C                   PARM      *BLANK        ##EDI             1
9    C                   PARM      *BLANK        ##WCDK            1
10   C                   PARM      *BLANK        ##RHAB            1
11   C                   PARM      *BLANK        ##MSS             1
12   C                   PARM      *BLANK        ##FSCH            1
13   C                   PARM      *BLANK        ##PRVB            1
14   C                   PARM      *BLANK        ##NPWC            1
15   C                   PARM      *BLANK        ##OS              1
16   C                   PARM      *BLANK        ##ARLS            1
17   C                   PARM      *BLANK        ##CCOD            1
18   C                   PARM      *BLANK        ##CRVW            1
19   C                   PARM      *BLANK        ##PROM            1
20   C                   PARM      *BLANK        ##MCRV            1
21   C                   PARM      *BLANK        ##FREQ            1
22   C                   PARM      *BLANK        ##CRAC            1
23   C                   PARM      *BLANK        ##PREO            1
24   C                   PARM      *BLANK        ##PARN            1
25   C                   PARM      *BLANK        ##MGRI            1
26   C                   PARM      *BLANK        ##KCIF            1
27   C                   PARM      *BLANK        ##CURS            1
28   C                   PARM      *BLANK        ##PCIB            1
29   C                   PARM      *BLANK        ##CLT#            3
30   C                   PARM      *BLANK        ##UCPF            1
31   C                   PARM      *BLANK        ##DCF             1
32   C                   PARM      *BLANK        ##AUTB            1
33   C                   PARM      *BLANK        ##DEIF            1
34   C                   PARM      *BLANK        ##AURE            1
35   C                   PARM      *BLANK        ##CRED            1
36   C                   PARM      *BLANK        ##IDRT            1
37   C                   PARM      *BLANK        ##CPRV            1
38   C                   PARM      *BLANK        ##EFT             1
39   C                   PARM      *BLANK        ##GOVR            1
40   C                   PARM      *BLANK        ##NCDC            1
41   C                   PARM      *BLANK        ##SPLT            1
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   MOVEL     'Y'           @@SPLT
     C     ##SPLT        IFEQ      'N'
     C                   MOVEL(P)  @@ST          KKKEY
     C     KKKEY         CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     STMLSP        ANDEQ     'N'
     C     @@FDAT        ANDGE     STMLDT
     C     @@BTYP        IFEQ      'N'
     C     STPHYS        ANDEQ     'Y'
     C     @@BTYP        OREQ      'H'
     C     STHOSP        ANDEQ     'Y'
     C     @@BTYP        OREQ      'D'
     C     STDENT        ANDEQ     'Y'
     C     @@BTYP        OREQ      'X'
     C     STPHYS        ANDEQ     'Y'
     C                   MOVEL     'N'           @@SPLT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   RETURN
