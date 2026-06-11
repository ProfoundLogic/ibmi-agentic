     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*
     H* PROGRAM: F7095R
     H* DESCRIPTION: CASE TRANSFER/DELETION - WRITE FACTP, FAT1P
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  06/09/20  C:295 Project : SY420                              *
     H*                  Mask SSN (Recompile only)                   *
     h*                                                               *
     H*  06/12/01 C:62  PROJECT: MCMNT
     H*                 CORRECT TRANSFER IF DOUBLE CASE DON'T CHANGE
     H*                 TO FYR.
     H*
     H*  02/24/99 C:94  PROJECT: SUP16647
     H*                 IF PROGRAM IS CALLED FROM WCMS THEN PUT
     H*                 @@FYR INTO CTTFYR
     H*
     H*  02/08/99 C:94  PROJECT: SUP16448
     H*                 DEL CALL TO SC900C AND PUT INTO F7096R
     H*
     H*  01/26/99 C:94  PROJECT: SUP16210
     H*                 CHANGED PROGRAM SO THAT IF YEAR OF ACCIDENT
     H*                 DATE WAS CHANGED THEN THE FYR WOULD BE CHANGED.
     H*
     H*  11/30/98 C:94  PROJECT: MCMNT
     H*                 CHANGED LENGTH OF ACDT PARM
     H*
     H*  11/20/98 C:94  PROJECT: SUP15110
     H*                 CHANGE WHAT FIELDS TO LOOK AT IF THEY ARE BLANK
     H*                 TO USE RRN#3
     H*
     H*  11/18/98 C:94  PROJECT: MCMNT
     H*                 CHANGE CMTIME AND CMITIM TO CTTIME AND CTITIM
     H*
     H*  11/11/98 C:94  PROJECT: MCMNT
     H*                 MOVE A 'T' OR A 'D' INTO STATUE FIELD
     H*
     H*****************************************************************
     H*
     F*
     FFMCMP     IF   E             DISK
     F*
     FSMUPL2    IF   E           K DISK
     F*
     FFACTP     UF A E           K DISK
     F                                     INFDS(DBFD1)
     F*
     FFACTL     IF   E           K DISK
     F                                     RENAME(PCT01:PCT02)
     F*
     FFAT1L     UF A E           K DISK
     F*
     FFAT3LQ    IF   E           K DISK
     F*
     FFMCDL     IF   E           K DISK

     D*
     D DBFD1           DS
     D  DBRRN1               397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     I*           DS
     I*                                       1  40 QFKEY
     I*                                       1   30CTFCO#
     I*                                       4   80CTFSUB
     I*                                       9  110CTFFND
     I*                                      12  140CTFFYR
     I*                                      15  210CTFCAS
     I*           DS
     I*                                       1  40 QTKEY
     I*                                       1   30CTTCO#
     I*                                       4   80CTTSUB
     I*                                       9  110CTTFND
     I*                                      12  140CTTFYR
     I*                                      15  210CTTCAS
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@SS#             9
     C*** BEG CHG *** 11/30/98 ****************************************
     C                   PARM                    @@ACDT            8 0
     C**********           PARM           @@ACDT  70
     C*** END CHG *** 11/30/98 ****************************************
     C                   PARM                    @@LSN            30
     C                   PARM                    @@FSN            15
     C                   PARM                    @@MID             1
     C                   PARM                    @@ADR1           40
     C                   PARM                    @@ADR2           40
     C                   PARM                    @@CTY            25
     C                   PARM                    @@ST              2
     C                   PARM                    @@ZIP             9 0
     C                   PARM                    @@DOB             7 0
     C                   PARM                    @@SEX             1
     C                   PARM                    @@USER           10
     C                   PARM                    @@CODE           10 0
     C                   PARM                    @@COM1           70
     C                   PARM                    @@COM2           70
     C                   PARM                    @@COM3           70
     C                   PARM                    @@FLAG            1
     C                   PARM                    @@TD              1
     C                   PARM                    @@RRN3            9 0
     C                   PARM                    @@WCMS            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     MUKY          KLIST
     C                   KFLD                    KKUSER
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CTKY1         KLIST
     C                   KFLD                    KKKEY
     C*
     C     CTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSTAT
     C*
     C     T1KY          KLIST
     C                   KFLD                    KKKEY             9 0
     C                   KFLD                    KKFILE
     C                   KFLD                    KKNOTE
     C                   KFLD                    KKSTAT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    Q1USER        KKUSER
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMSTAT        KKSTAT
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    CMEMP#        KKEMP#
     C     *LIKE         DEFINE    CMDIV         KKDIV
     C     *LIKE         DEFINE    CMLSN         KKLSN
     C     *LIKE         DEFINE    CMFSN         KKFSN
     C     *LIKE         DEFINE    CMMID         KKMID
     C     *LIKE         DEFINE    CDADR1        KKADR1
     C     *LIKE         DEFINE    CDADR2        KKADR2
     C     *LIKE         DEFINE    CDCTY         KKCTY
     C     *LIKE         DEFINE    CDST          KKST
     C     *LIKE         DEFINE    CDZIP         KKZIP
     C     *LIKE         DEFINE    CDDTOB        KKDOB
     C     *LIKE         DEFINE    CDSEX         KKSEX
     C     *LIKE         DEFINE    TQFILE        KKFILE
     C     *LIKE         DEFINE    TQNOTE        KKNOTE
     C     *LIKE         DEFINE    TQSTAT        KKSTA
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     @@WCMS        IFEQ      'Y'
     C     @@FLAG        ANDEQ     'X'
     C*** BEG DEL *** 11/20/98 ****************************************
     C**********
     C********** @@EMP#    IFEQ *ZEROS
     C********** @@DIV     OREQ *ZEROS
     C********** @@SS#     OREQ *BLANKS
     C********** @@ACDT    OREQ *ZEROS
     C********** @@FSN     OREQ *BLANKS
     C********** @@MID     OREQ *BLANK
     C********** @@LSN     OREQ *BLANKS
     C********** @@RRN3    CHAINPCM01                99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDCMEMP#    @@EMP#
     C**********           Z-ADDCMDIV     @@DIV
     C**********           MOVE CMSS#     @@SS#
     C**********           Z-ADDCMACDT    @@ACDT
     C**********           MOVELCMFSN     @@FSN     P
     C**********           MOVELCMMID     @@MID
     C**********           MOVELCMLSN     @@LSN     P
     C**********           ENDIF
     C**********           ENDIF
     C*** END DEL *** 11/20/98 ****************************************
     C*
     C                   MOVE      @@SS#         KKSS#
     C     @@ADR1        IFEQ      *BLANKS
     C     @@CTY         OREQ      *BLANKS
     C     @@ST          OREQ      *BLANKS
     C     @@ZIP         OREQ      *ZEROS
     C********** @@DOB     OREQ *ZEROS
     C     @@SEX         OREQ      *BLANK
     C*
     C     CDKY          CHAIN     PCD01                              99
     C                   MOVE      CDADR1        @@ADR1
     C                   MOVE      CDADR2        @@ADR2
     C                   MOVE      CDCTY         @@CTY
     C                   MOVE      CDST          @@ST
     C                   MOVE      CDZIP         @@ZIP
     C**********           Z-ADDCDDTOB    @@DOB
     C                   MOVE      CDSEX         @@SEX
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C                   MOVE      *ZEROS        KKCO#
     C                   MOVE      *ZEROS        KKSUB#
     C                   MOVE      *ZEROS        KKFND
     C                   MOVE      *ZEROS        KKFYR
     C                   MOVE      *ZEROS        KKCASE
     C                   MOVE      *BLANK        KKSTAT
     C                   MOVE      *BLANKS       KKSS#
     C                   MOVE      *ZEROS        KKACDT
     C                   MOVE      *ZEROS        KKEMP#
     C                   MOVE      *ZEROS        KKDIV
     C                   MOVE      *BLANKS       KKLSN
     C                   MOVE      *BLANKS       KKFSN
     C                   MOVE      *BLANKS       KKMID
     C                   MOVE      *BLANKS       KKADR1
     C                   MOVE      *BLANKS       KKADR2
     C                   MOVE      *BLANKS       KKCTY
     C                   MOVE      *BLANKS       KKST
     C                   MOVE      *ZEROS        KKZIP
     C                   Z-ADD     *ZEROS        KKDOB
     C                   MOVE      *BLANK        KKSEX
     C*
     C     @@RRN         CHAIN(N)  PCM01                              99
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C                   MOVE      CMSTAT        KKSTAT
     C                   MOVE      CMSS#         KKSS#
     C                   MOVE      CMACDT        KKACDT
     C                   MOVE      CMEMP#        KKEMP#
     C                   MOVE      CMDIV         KKDIV
     C                   MOVE      CMLSN         KKLSN
     C                   MOVE      CMFSN         KKFSN
     C                   MOVE      CMMID         KKMID
     C*
     C     CDKY          CHAIN     PCD01                              99
     C                   MOVE      CDADR1        KKADR1
     C                   MOVE      CDADR2        KKADR2
     C                   MOVE      CDCTY         KKCTY
     C                   MOVE      CDST          KKST
     C                   MOVE      CDZIP         KKZIP
     C                   Z-ADD     CDDTOB        KKDOB
     C                   MOVE      CDSEX         KKSEX
     C*
     C                   EXSR      MVFACT
     C*
     C                   EXSR      READ
     C*
     C     *IN95         DOWNE     *ON
     C*
     C                   EXSR      MVFAT1
     C*
     C                   EXSR      READ
     C*
     C                   ENDDO
     C*
     C                   MOVEL(P)  @@USER        KKUSER
     C     MUKY          CHAIN     PUP01                              99
     C                   MOVE      UPOUTQ        ##OUTQ           10
     C                   MOVE      'D'           ##MODE            1
     C*
     C                   CALL      'F7095C'
     C                   PARM                    ##RRN1            9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      CTKEY         ##KEY             9 0
     C                   PARM      @@FLAG        ##FLAG            1
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM                    ##OUTQ
     C                   PARM                    ##MODE
     C*
     C*** BEG DEL *** 02/08/99 ***************************************
     C**********           MOVE *BLANK    QQRTCD
     C**********
     C********** ##WCIF    IFEQ 'Y'
     C********** ##WCIN    ANDEQ'Y'
     C**********           MOVE CMCO#     #1CO#
     C**********           MOVE CMSUB#    #1SUB#
     C**********           MOVE CMFND     #1FND
     C**********
     C**********           CALL 'FA701R'
     C**********           PARM *BLANKS   ##RTCD  1
     C**********           PARM           #1CO#   3
     C**********           PARM           #1SUB#  5
     C**********           PARM           #1FND   3
     C**********           PARM *BLANKS   ##OUTS  1
     C**********
     C********** ##OUTS    IFEQ 'Y'
     C**********           MOVE 'Y'       ALLOW   1
     C**********           ELSE
     C**********           MOVE 'N'       ALLOW
     C**********           ENDIF
     C**********
     C**********           ELSE
     C**********           MOVE 'Y'       ALLOW
     C**********           ENDIF
     C**********
     C********** ALLOW     IFEQ 'Y'
     C**********
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C**********           MOVE CMCO#     #1CO#
     C**********           MOVE CMSUB#    #1SUB#
     C**********           MOVE CMFND     #1FND
     C**********           MOVE CMFYR     #1FYR
     C**********           MOVE CMCASE    #1CASE         90
     C**********
     C**********           CALL 'FA703R'
     C**********           PARM 'K'       ##RTCD  1
     C**********           PARM           ##WCIN  1
     C**********           PARM           ##WCDK  1
     C**********           PARM           #1CO#   3
     C**********           PARM           #1SUB#  5
     C**********           PARM           #1FND   3
     C**********           PARM           #1FYR   3
     C**********           PARM           #1CASE  7
     C**********           PARM *BLANK    ##KEY1 40
     C**********
     C**********           MOVE CTTCO#    #1CO#
     C**********           MOVE CTTSUB    #1SUB#
     C**********           MOVE CTTFND    #1FND
     C**********           MOVE CTTFYR    #1FYR
     C**********           MOVE CTTCAS    #1CASE         90
     C**********
     C**********           CALL 'FA703R'
     C**********           PARM 'K'       ##RTCD  1
     C**********           PARM           ##WCIN  1
     C**********           PARM           ##WCDK  1
     C**********           PARM           #1CO#   3
     C**********           PARM           #1SUB#  5
     C**********           PARM           #1FND   3
     C**********           PARM           #1FYR   3
     C**********           PARM           #1CASE  7
     C**********           PARM *BLANK    ##KEY2 40
     C**********
     C**********           CALL 'SC900C'
     C**********           PARM ##KEY1    QFKEY  40
     C**********           PARM ##KEY2    QTKEY  40
     C**********           PARM *BLANKS   QQQUE  10
     C**********           PARM 'B'       QQMODE  1
     C**********           PARM ##RTCD    QQRTCD  1
     C**********
     C**********           ENDIF
     C**********
     C*** END DEL *** 02/08/99 ***************************************
     C                   MOVE      '1'           *INLR
     C*
     C*================================================================
     C* MVFACT - Move Information into From and To fields in FACTP
     C*================================================================
     C     MVFACT        BEGSR
     C*
     C                   Z-ADD     *ZEROS        ##RRN1
     C                   Z-ADD     *ZEROS        CTFCO#
     C                   Z-ADD     *ZEROS        CTFSUB
     C                   Z-ADD     *ZEROS        CTFFND
     C                   Z-ADD     *ZEROS        CTFFYR
     C                   Z-ADD     *ZEROS        CTFCAS
     C                   MOVE(P)   *BLANKS       CTFSS#
     C                   Z-ADD     *ZEROS        CTFACD
     C                   Z-ADD     *ZEROS        CTFEMP
     C                   Z-ADD     *ZEROS        CTFDIV
     C                   MOVE(P)   *BLANKS       CTFLSN
     C                   MOVE(P)   *BLANKS       CTFFSN
     C                   MOVE(P)   *BLANK        CTFMID
     C                   MOVE(P)   *BLANKS       CTFAD1
     C                   MOVE(P)   *BLANKS       CTFAD2
     C                   MOVE(P)   *BLANKS       CTFCTY
     C                   MOVE(P)   *BLANKS       CTFST
     C                   Z-ADD     *ZEROS        CTFZIP
     C                   Z-ADD     *ZEROS        CTFDOB
     C                   MOVE(P)   *BLANK        CTFSEX
     C*
     C*** MOVE INFORMATION INTO TO FIELDS ***
     C*
     C                   Z-ADD     *ZEROS        CTTCO#
     C                   Z-ADD     *ZEROS        CTTSUB
     C                   Z-ADD     *ZEROS        CTTFND
     C                   Z-ADD     *ZEROS        CTTFYR
     C                   Z-ADD     *ZEROS        CTTCAS
     C                   MOVE(P)   *BLANKS       CTTSS#
     C                   Z-ADD     *ZEROS        CTTACD
     C                   Z-ADD     *ZEROS        CTTEMP
     C                   Z-ADD     *ZEROS        CTTDIV
     C                   MOVE(P)   *BLANKS       CTTLSN
     C                   MOVE(P)   *BLANKS       CTTFSN
     C                   MOVE(P)   *BLANK        CTTMID
     C                   MOVE(P)   *BLANKS       CTTAD1
     C                   MOVE(P)   *BLANKS       CTTAD2
     C                   MOVE(P)   *BLANKS       CTTCTY
     C                   MOVE(P)   *BLANKS       CTTST
     C                   Z-ADD     *ZEROS        CTTZIP
     C                   Z-ADD     *ZEROS        CTTDOB
     C                   MOVE(P)   *BLANK        CTTSEX
     C                   MOVE(P)   *BLANKS       CTRQST
     C                   MOVE      *BLANKS       CTREAS
     C                   MOVE(P)   *BLANKS       CTCOM1
     C                   MOVE(P)   *BLANKS       CTCOM2
     C                   MOVE(P)   *BLANKS       CTCOM3
     C*
     C                   MOVE      *HIVAL        KKKEY
     C     CTKY1         SETGT     PCT02                              99
     C                   READP     PCT02                                  98
     C                   MOVE      CTKEY         KKKEY
     C                   CLEAR                   PCT01
     C*
     C*** MOVING INTO FROM FIELDS ***
     C*
     C                   Z-ADD     KKCO#         CTFCO#
     C                   Z-ADD     KKSUB#        CTFSUB
     C                   Z-ADD     KKFND         CTFFND
     C                   Z-ADD     KKFYR         CTFFYR
     C                   Z-ADD     KKCASE        CTFCAS
     C                   MOVEL(P)  KKSS#         CTFSS#
     C*** BEG DEL *** 01/26/99 ***************************************
     C********** @@WCMS    IFEQ 'N'
     C**********           Z-ADDKKACDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     CTFACD
     C**********           ELSE
     C*** END DEL *** 01/26/99 ***************************************
     C                   Z-ADD     KKACDT        CTFACD
     C*** BEG DEL *** 01/26/99 ***************************************
     C**********           ENDIF
     C*** END DEL *** 01/26/99 ***************************************
     C                   Z-ADD     KKEMP#        CTFEMP
     C                   Z-ADD     KKDIV         CTFDIV
     C                   MOVEL(P)  KKLSN         CTFLSN
     C                   MOVEL(P)  KKFSN         CTFFSN
     C                   MOVEL(P)  KKMID         CTFMID
     C                   MOVEL(P)  KKADR1        CTFAD1
     C                   MOVEL(P)  KKADR2        CTFAD2
     C                   MOVEL(P)  KKCTY         CTFCTY
     C                   MOVEL(P)  KKST          CTFST
     C                   Z-ADD     KKZIP         CTFZIP
     C                   Z-ADD     KKDOB         CTFDOB
     C                   MOVEL(P)  KKSEX         CTFSEX
     C*
     C*** MOVE INFORMATION INTO TO FIELDS ***
     C*
     C                   Z-ADD     @@CO#         CTTCO#
     C                   Z-ADD     @@SUB#        CTTSUB
     C                   Z-ADD     @@FND         CTTFND
     C*** BEG CHG *** 02/24/99 ***************************************
     C     @@WCMS        IFEQ      'Y'
     C                   Z-ADD     @@FYR         CTTFYR
     C                   ENDIF
     C**********           Z-ADD@@FYR     CTTFYR
     C*** END CHG *** 02/24/99 ***************************************
     C                   Z-ADD     @@CASE        CTTCAS
     C                   MOVEL(P)  @@SS#         CTTSS#
     C********** @@WCMS    IFEQ 'N'
     C                   Z-ADD     @@ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTTACD
     C**********           ELSE
     C**********           Z-ADD@@ACDT    CTTACD
     C**********           ENDIF
     C*** BEG ADD *** 02/24/99 ***************************************
     C     @@WCMS        IFNE      'Y'
     C*** END ADD *** 02/24/99 ***************************************
     C*** BEGIN ADD *** 06/12/01 **************************************
     C     @@FLAG        IFEQ      'X'
     C*** END   ADD *** 06/12/01 **************************************
     C*** BEG ADD *** 01/26/99 ***************************************
     C                   MOVEL     CTTACD        TEMFYR            3 0
     C     TEMFYR        IFNE      CTTFYR
     C                   Z-ADD     TEMFYR        CTTFYR
     C                   ENDIF
     C*** END ADD *** 01/26/99 ***************************************
     C*** BEGIN ADD *** 06/12/01 **************************************
     C                   ELSE
     C                   Z-ADD     @@FYR         CTTFYR
     C                   ENDIF
     C*** END   ADD *** 06/12/01 **************************************
     C*** BEG ADD *** 02/24/99 ***************************************
     C                   ENDIF
     C*** END ADD *** 02/24/99 ***************************************
     C                   Z-ADD     @@EMP#        CTTEMP
     C                   Z-ADD     @@DIV         CTTDIV
     C                   MOVEL(P)  @@LSN         CTTLSN
     C                   MOVEL(P)  @@FSN         CTTFSN
     C                   MOVEL(P)  @@MID         CTTMID
     C                   MOVEL(P)  @@ADR1        CTTAD1
     C                   MOVEL(P)  @@ADR2        CTTAD2
     C                   MOVEL(P)  @@CTY         CTTCTY
     C                   MOVEL(P)  @@ST          CTTST
     C                   Z-ADD     @@ZIP         CTTZIP
     C                   Z-ADD     @@DOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTTDOB
     C                   MOVEL(P)  @@SEX         CTTSEX
     C                   MOVEL(P)  @@USER        CTRQST
     C                   MOVEL     @@CODE        CTREAS
     C                   MOVEL(P)  @@COM1        CTCOM1
     C                   MOVEL(P)  @@COM2        CTCOM2
     C                   MOVEL(P)  @@COM3        CTCOM3
     C                   ADD       1             KKKEY
     C                   MOVEL     KKKEY         CTKEY
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        CTIUSR
     C                   MOVEL     Q1PGM         CTIPGM
     C*** BEG CHG *** 11/19/98 ***************************************
     C                   MOVEL     DSTIME        CTITIM
     C**********           MOVELDSTIME    CMITIM
     C*** END CHG *** 11/19/98 ***************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTIDT
     C*
     C                   MOVEL     Q1USER        CTUSER
     C                   MOVEL     Q1PGM         CTPGM
     C*** BEG CHG *** 11/19/98 ***************************************
     C                   MOVEL     DSTIME        CTTIME
     C***********          MOVELDSTIME    CMTIME
     C*** END CHG *** 11/19/98 ***************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C*** BEG ADD *** 11/11/98 ***************************************
     C                   MOVE      @@TD          CTSTAT
     C*** END ADD *** 11/11/98 ***************************************
     C*
     C                   WRITE     PCT01
     C                   Z-ADD     DBRRN1        ##RRN1
     C*
     C                   SETOFF                                       50
     C     *IN50         IFEQ      *ON
     C                   READ      PCT01                                  99
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVFAT1 - Move Information into FAT1P
     C*================================================================
     C     MVFAT1        BEGSR
     C*
     C     T1KY          CHAIN     PT101                              99
     C*
     C*** MOVING INTO FIELDS ***
     C*
     C                   MOVEL     CTKEY         T1KEY
     C                   MOVEL     KKFILE        T1FILE
     C                   MOVEL     KKNOTE        T1NOTE
     C                   MOVEL(P)  @@TD          T1STAT
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        T1IUSR
     C                   MOVEL     Q1PGM         T1IPGM
     C                   MOVEL     DSTIME        T1ITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         T1IDT
     C*
     C                   MOVEL     Q1USER        T1USER
     C                   MOVEL     Q1PGM         T1PGM
     C                   MOVEL     DSTIME        T1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         T1DT
     C*
     C                   WRITE     PT101
     C*
     C                   ELSE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        T1USER
     C                   MOVEL     Q1PGM         T1PGM
     C                   MOVEL     DSTIME        T1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         T1DT
     C*
     C                   UPDATE    PT101
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C                   READ      PT302                                  95
     C                   MOVEL     TQFILE        KKFILE
     C                   MOVEL     TQNOTE        KKNOTE
     C                   MOVEL     TQSTAT        KKSTA
     C*
     C                   ENDSR
