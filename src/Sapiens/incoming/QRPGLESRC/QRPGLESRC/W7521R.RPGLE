     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7521R - CERT HOLDER ROLLOVER
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/04/09  C:61   PROJECT: WC1304                              *
     H*                  Add processing for "Out of Country"          *
     H*                  Cert Holder address.                         *
     H*                                                               *
     H* 03/09/09  C:190  PROJECT: WC1750                              *
     H*                  The program has been restructured so that    *
     H*                  it will behave depending on from which       *
     H*                  option it's being called. If it's being      *
     H*                  called from the separate Rollover screen     *
     H*                  then functioning will be based on 'From'     *
     H*                  and 'To Date' parameters and if being        *
     H*                  called from W/W Certificate Holders then     *
     H*                  existing functionality should go on.         *
     H*                                                               *
     H* 05/19/05  C:33   Seq#: WA102/46532                            *
     H*                  Do not roll Inactive certificates.           *
     H*                                                               *
     H* 12/15/98  C:35   PROJECT: SUPPORT WA102 #15543                *
     H*                  PROGRAM WAS NOT PROCESSING WHEN RUN FOR ALL  *
     H*                  EMPLOYERS.                                   *
     H*                                                               *
     H*  10/29/97  C:33  PROJECT: WC577                               *
     H*                  Recompiled due to fields being added to WMCEP*
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F***  Begin Add  ***  03/09/09  **************************************
     FWDELL4    IF   E           K DISK
     F                                     RENAME(PEL01:PEL04)
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PELLL)
     F***  End   Add  ***  03/09/09  **************************************
     FWMCEL     UF A E           K DISK

     D***  End   Add  ***  03/09/09  **************************************
     D*   DEFINE DATA STRUCTURES
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I***  Begin Add  ***  03/09/09  **************************************
     IPEL04
     I              ELCO#                       E4CO#
     I              ELFND                       E4FND
     I              ELFYR                       E4FYR
     I              ELEMP#                      E4EMP#
     I              ELDIV                       E4DIV
     I              ELPFDT                      E4PFDT
     I              ELPTDT                      E4PTDT

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1NFYR
     C                   KFLD                    CEEMP#
     C                   KFLD                    CEDIV
     C*
     C*   KEY LIST FOR THE CERTIFICATE MASTER FILE
     C*
     C     CEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     CEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCER#
     C*
     C     CEKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1NFYR
     C                   KFLD                    CEEMP#
     C                   KFLD                    CEDIV
     C                   KFLD                    CECER#
     C*
     C     CEKEY4        KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1EMP#
     C                   KFLD                    X1DIV
     C                   KFLD                    X1CER#
     C***  Begin Add  ***  03/09/09  **************************************
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1FTDT
      *
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     CEKEY5        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E4FYR
     C                   KFLD                    E4EMP#
     C                   KFLD                    E4DIV
     C*
     C     CEKEY6        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    NWYR
     C                   KFLD                    CEEMP#
     C                   KFLD                    CEDIV
     C                   KFLD                    CECER#
     C*
     C     CEKEY7        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E4FYR
     C                   KFLD                    CEEMP#
     C*
     C     ELKEY5        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CEEMP#
     C                   KFLD                    CEDIV
     C                   KFLD                    NWYR
     C*
     C     ELKEY0        KLIST
     C                   KFLD                    E4CO#
     C                   KFLD                    E4FND
     C                   KFLD                    E4EMP#
     C                   KFLD                    E4DIV
     C                   KFLD                    E4FYR
     C*
     C     ELKY01        KLIST
     C                   KFLD                    E4CO#
     C                   KFLD                    E4FND
     C                   KFLD                    E4EMP#
     C                   KFLD                    E4DIV
     C***  End   Add  ***  03/09/09  **************************************
      *
     C*
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELEMP#        X1EMP#
     C     *LIKE         DEFINE    ELDIV         X1DIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQCER#            5
     C                   PARM                    QQNFYR            3
     C***  Begin Add  ***  03/09/09  **************************************
     C                   PARM                    QQFTDT            8
     C                   PARM                    QQTTDT            8
     C***  End   Add  ***  03/09/09  **************************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQCER#        XXCER#            5 0
     C                   MOVE      QQNFYR        XXNFYR            3 0
     C                   Z-ADD     XXNFYR        X1NFYR            3 0
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C                   MOVE      '0'           *IN40
     C                   MOVE      '0'           *IN41
     C*
      **** BEGIN CHG **** 12/15/98 *********************************************
     C********** QQEMP#    IFEQ '00000'
     C     QQEMP#        IFEQ      *ZEROS
      **** END   CHG **** 12/15/98 *********************************************
     C                   MOVE      '1'           *IN40
     C                   Z-ADD     0             XXDIV
     C                   END
     C     QQCER#        IFEQ      '00000'
     C                   MOVE      '1'           *IN41
     C                   END
     C***  Begin Add  ***  03/09/09  *********************************
      *  DEFINE FROM & TO DATES FOR ASCENDING ORDER COMPARISON
      *
     C                   Z-ADD     0             X1FTDT            7 0          FROM DATE
     C                   Z-ADD     0             X1TTDT            7 0          TO DATE
      *
     C     Q1PRMS        IFGE      7
     C                   MOVE      QQFTDT        XXFTDT            8 0
     C                   MOVE      QQTTDT        XXTTDT            8 0
      *
     C                   Z-ADD     XXFTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FTDT
      *
     C                   Z-ADD     XXTTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TTDT
     C                   ENDIF
     C***  End   Add  ***  03/09/09  *********************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*  GET THE USER PARAMETER INFORMATION
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANKS       UPUSNM
     C*
     C*   CHAIN TO CERTIFICATE HOLDER FILE FOR ALL EMPLOYERS FOR FUND
     C*
     C   40
     CAN 41CEKEY         CHAIN     PCE01                              50
     C*
     C*   CHAIN TO CERTIFICATE FILE FOR ONE EMPLOYERS ALL CERTIFICATES
     C*
     C  N40
     CAN 41CEKEY1        CHAIN     PCE01                              50
     C*
     C*   CHAIN TO CERTIFICATE FILE FOR ONE EMPLOYER ONE CERTIFICATE
     C*
     C  N40
     CANN41CEKEY2        CHAIN     PCE01                              50
     C*
     C***  Begin Add  ***  03/09/09  **************************************
     C     X1FTDT        IFNE      0
     C     X1TTDT        ANDNE     0
     C     Q1PRMS        ANDGT     7
     C                   EXSR      PRCDAT
     C                   ELSE
     C***  End   Add  ***  03/09/09  **************************************
     C     *IN50         DOWEQ     '0'
     C                   EXCEPT    PCE01E
     C*
     C*  SAVE RECORD KEY FIELDS
     C*
     C                   Z-ADD     CECO#         X1CO#             3 0
     C                   Z-ADD     CEFND         X1FND             3 0
     C                   Z-ADD     CEFYR         X1FYR             3 0
     C                   Z-ADD     CEEMP#        X1EMP#
     C                   Z-ADD     CEDIV         X1DIV
     C                   Z-ADD     CECER#        X1CER#            5 0
     C*
     C     CERLFL        IFEQ      'Y'
     C***  Begin Add  ***  05/19/05  **************************************
     C     CESTAT        ANDNE     'I'
     C***  End   Add  ***  05/19/05  **************************************
     C*
     C*  MOVE RECORD TO WORK FIELDS
     C*
     C                   MOVEL     CENAME        XXNAME           40
     C                   MOVEL     CEADR1        XXADR1           40
     C                   MOVEL     CEADR2        XXADR2           40
     C                   MOVEL     CECTY         XXCTY            25
     C                   MOVEL     CEST          XXST              2
     C                   MOVE      CEZIP         XXZIP             9 0
     C                   MOVEL     CEATTN        XXATTN           60
      ***** BEG ADD ******* 04/04/09 **********************************
     C                   MOVEL(P)  CECTRY        XXCTRY            5
     C                   MOVEL(P)  CEPROV        XXPROV           40
      ***** END ADD ******* 04/04/09 **********************************
     C                   MOVEL     CERLFL        XXRLFL            1
     C                   MOVEL     CEREM1        XXREM1           60
     C                   MOVEL     CEREM2        XXREM2           60
     C                   MOVEL     CEREM3        XXREM3           60
     C                   MOVEL     CEREM4        XXREM4           60
     C                   MOVEL     CEREM5        XXREM5           60
     C                   MOVE      CESTAT        XXSTAT            1
     C*
     C*  CHECK EMPLOYER FILE TO BE SURE NEW FUND YEAR IS VALID
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  UPDATE OR WRITE RECORD FOR NEW FUND YEAR
     C*
     C     CEKEY3        CHAIN     PCE01                              51
     C                   Z-ADD     XXNFYR        CEFYR
     C                   MOVEL     XXNAME        CENAME
     C                   MOVEL     XXADR1        CEADR1
     C                   MOVEL     XXADR2        CEADR2
     C                   MOVEL     XXCTY         CECTY
     C                   MOVEL     XXST          CEST
     C                   MOVE      XXZIP         CEZIP
     C                   MOVEL     XXATTN        CEATTN
      ***** BEG ADD ******* 04/04/09 **********************************
     C                   MOVEL(P)  XXCTRY        CECTRY
     C                   MOVEL(P)  XXPROV        CEPROV
      ***** END ADD ******* 04/04/09 **********************************
     C                   MOVEL     XXRLFL        CERLFL
     C                   MOVEL     XXREM1        CEREM1
     C                   MOVEL     XXREM2        CEREM2
     C                   MOVEL     XXREM3        CEREM3
     C                   MOVEL     XXREM4        CEREM4
     C                   MOVEL     XXREM5        CEREM5
     C                   MOVE      XXSTAT        CESTAT
     C                   MOVEL     Q1USER        CEUSER
     C                   MOVE      Q1PGM         CEPGM
     C                   MOVE      DSTIME        CETIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CEDT
     C*
     C     *IN51         IFEQ      '0'
     C                   UPDATE    PCE01
     C                   ELSE
     C                   WRITE     PCE01
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   REPOSITION AT ORIGINAL RECORD
     C*
     C     CEKEY4        CHAIN     PCE01                              50
     C*
     C*   GET NEXT CERTIFICATE HOLDER FILE IF NECESSARY
     C*
     C   40
     CAN 41CEKEY         READE     PCE01                                  50
     C  N40
     CAN 41CEKEY1        READE     PCE01                                  50
     C  N40
     CANN41              MOVE      '1'           *IN50
     C                   END
     C*
     C***  Begin Add  ***  03/09/09  **************************************
     C                   ENDIF                                                  Q1PRMS > 7
     C***  End   Add  ***  03/09/09  **************************************
     C*   END PROGRAM
     C*
     C                   MOVE      '1'           *INLR
      **** Begin Add **** 03/09/09 ***********************************
      ****************************************************************
      *  CERT OF INS-ROLLOVER BY EXP DATE
      ****************************************************************
     C     PRCDAT        BEGSR
      *
      * READ EMPLOYER DETAIL RECORDS
      *
     C     ELKY2         SETLL     PEL04
     C     ELKY3         READE     PEL04                                  40
      *
     C     *IN40         DOWEQ     *OFF
     C     E4PTDT        ANDLE     X1TTDT
      *
     C*   CHAIN TO CERTIFICATE FILE FOR ONE EMPLOYER ALL CERTIFICATE
     C*
     C     CEKEY5        CHAIN     PCE01                              50
     C*
     C     *IN50         DOWEQ     '0'
     C                   EXCEPT    PCE01E
     C*
     C*  SAVE RECORD KEY FIELDS
     C*
     C                   Z-ADD     CECO#         X1CO#             3 0
     C                   Z-ADD     CEFND         X1FND             3 0
     C                   Z-ADD     CEFYR         X1FYR             3 0
     C                   Z-ADD     CEEMP#        X1EMP#
     C                   Z-ADD     CEDIV         X1DIV
     C                   Z-ADD     CECER#        X1CER#            5 0
     C*
     C     CERLFL        IFEQ      'Y'
     C     CESTAT        ANDNE     'I'
     C*
     C*  MOVE RECORD TO WORK FIELDS
     C*
     C                   MOVEL     CENAME        XXNAME           40
     C                   MOVEL     CEADR1        XXADR1           40
     C                   MOVEL     CEADR2        XXADR2           40
     C                   MOVEL     CECTY         XXCTY            25
     C                   MOVEL     CEST          XXST              2
     C                   MOVE      CEZIP         XXZIP             9 0
     C                   MOVEL     CEATTN        XXATTN           60
      ***** BEG ADD ******* 04/04/09 **********************************
     C                   MOVEL(P)  CECTRY        XXCTRY
     C                   MOVEL(P)  CEPROV        XXPROV
      ***** END ADD ******* 04/04/09 **********************************
     C                   MOVEL     CERLFL        XXRLFL            1
     C                   MOVEL     CEREM1        XXREM1           60
     C                   MOVEL     CEREM2        XXREM2           60
     C                   MOVEL     CEREM3        XXREM3           60
     C                   MOVEL     CEREM4        XXREM4           60
     C                   MOVEL     CEREM5        XXREM5           60
     C                   MOVE      CESTAT        XXSTAT            1
     C*
     C*  CHECK EMPLOYER FILE TO BE SURE NEW FUND YEAR IS VALID
     C*
     C     ELKEY0        SETGT     PELLL
     C     ELKY01        READE     PELLL                                  40
     C                   MOVE      ELFYR         NWYR              3 0
     C*
     C     *IN40         IFEQ      '0'
     C*
     C*  UPDATE OR WRITE RECORD FOR NEW FUND YEAR
     C*
     C     CEKEY6        CHAIN     PCE01                              51
     C                   Z-ADD     NWYR          CEFYR
     C                   MOVEL     XXNAME        CENAME
     C                   MOVEL     XXADR1        CEADR1
     C                   MOVEL     XXADR2        CEADR2
     C                   MOVEL     XXCTY         CECTY
     C                   MOVEL     XXST          CEST
     C                   MOVE      XXZIP         CEZIP
     C                   MOVEL     XXATTN        CEATTN
      ***** BEG ADD ******* 04/04/09 **********************************
     C                   MOVEL(P)  XXCTRY        CECTRY
     C                   MOVEL(P)  XXPROV        CEPROV
      ***** END ADD ******* 04/04/09 **********************************
     C                   MOVEL     XXRLFL        CERLFL
     C                   MOVEL     XXREM1        CEREM1
     C                   MOVEL     XXREM2        CEREM2
     C                   MOVEL     XXREM3        CEREM3
     C                   MOVEL     XXREM4        CEREM4
     C                   MOVEL     XXREM5        CEREM5
     C                   MOVE      XXSTAT        CESTAT
     C                   MOVEL     Q1USER        CEUSER
     C                   MOVE      Q1PGM         CEPGM
     C                   MOVE      DSTIME        CETIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CEDT
     C*
     C     *IN51         IFEQ      '0'
     C                   UPDATE    PCE01
     C                   ELSE
     C                   WRITE     PCE01
     C                   ENDIF                                                  *IN51 = '0'
     C*
     C                   ENDIF                                                  *IN60 = '0'
     C*
     C                   ENDIF                                                  CERLFL IFEQ 'Y'
     C*
     C*   REPOSITION AT ORIGINAL RECORD
     C*
     C     CEKEY4        CHAIN     PCE01                              50
     C*
     C     CEKEY7        READE     PCE01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C*
     C     ELKY3         READE     PEL04                                  40
     C                   ENDDO                                                  *IN40 DOWEQ
     C*
     C                   ENDSR                                                  PRCDAT
      **** End   Add **** 03/09/09 ***********************************
     OPCE01     E            PCE01E
