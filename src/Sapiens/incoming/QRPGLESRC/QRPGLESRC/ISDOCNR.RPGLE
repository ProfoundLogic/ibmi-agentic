     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE ISDOCNR  NEW DOC NOTES RECORD
     H*********************************************************************
     H*                                                                   *
     H*                                                                   *
     H*    CALLS TO:                                                      *
     H*                                                                   *
     H*                                                                   *
     H*********************************************************************
     H*  REVISIONS:                                                       *
     H*                                                                   *
     H* 07/11/19 C:062 Ticket: 80853(152)                                 *
     H*                Populate Q#CM75 with DNCM75 insrtead of blanks     *
     H*                                                                   *
     H* 09/09/11 C:255 SUPPORT: WA045 75700                               *
     H*                         UPDATE FLAG CMIQUE = 'Y'.                 *
     H*                         REMOVED FILE SMARL & CALLED PROGRAM       *
     H*                         DA701R INSTEAD.                           *
     H*                                                                   *
     H* 03/06/08 C:062 PROJECT: IM010 62408
     H*                CORRECT CDAT.
     H*                                                                   *
      * 10/14/03 C:128 PROJECT: SUPPORT                               *
      *                Changedto write dates to files in the correct  *
      *                format.                                        *
      *
      *** 02 05/15/03
      *               UPDATE ANY ADDITIONAL REMARKS FOR ALREADY INDEXED
      *               ITEMS.
      *                                                                   *
     H*********************************************************************
     H* INDICATOR USAGE                                                   *
     H*                                                                   *
     H* 60  GENERAL CHAIN INDICATOR                                       *
     H* 61  GENERAL CHAIN INDICATOR                                       *
     H*********************************************************************
     H** FILES:                                                          **
     H*********************************************************************
     F*
      * DOCUMENT NOTES FILE
     FIMDNL     IF A E           K DISK
     F**  ADD 05/15/03 *********************************************
     F**  BEG DEL *****  09/09/11 **********************************
     F******SMARL   UF  E           K        DISK                      A
     F**  END DEL *****  09/09/11 **********************************
     FIMDOLD    IF   E           K DISK
     F**  BEG CHG *****  09/09/11 **********************************
     F********SDCMLA  IF  E           K        DISK
     FSDCMLA    UF   E           K DISK
     F**  END CHG *****  09/09/11 **********************************
     F**  END ADD 05/15/03 ******************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XCURDT                 1      8  0
     D  XCURYR                 1      4  0
     D  XCURMD                 5      8  0
     D*
     D                 DS
     D  D2DTTM                 1     14  0
     D  D2TIME                 1      6  0
     D  D2DATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** DOC   NOTES (HISTORY) FILE
     C*
     C     DNKY1         KLIST
     C                   KFLD                    QQDCID
     C                   KFLD                    XXASQ#            5 0
     C*
     C*
     C     DNKY2         KLIST
     C                   KFLD                    QQDCID
     C** 05/15/03 ADD *********************************8
     C*
     C     ARKY          KLIST
     C                   KFLD                    DODKEY
     C                   KFLD                    DODSQ#
     C*
     C*
     C     ARK$$         KLIST
     C                   KFLD                    DODKEY
     C                   KFLD                    DODSQ#
     C                   KFLD                    $$ASQ#            5 0
     C*
     C     ARKY2         KLIST
     C                   KFLD                    DODKEY
     C***
     C     DOK           KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C     CMK           KLIST
     C                   KFLD                    DODKEY
     C                   KFLD                    DODSQ#
     C*
     C*
     C** 05/15/03 END ADD *****************************
     C*
      *** Begin Change 10/14/03 ***************************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *** End   Change 10/14/03 ***************************************
     C*** BEGIN ADD *** 03/06/08 **************************************
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END   ADD *** 03/06/08 **************************************
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM                    QQPNAM           10              CALLING PGM
     C                   PARM                    QQPRFL            1              PROTECT FLG
     C                   PARM                    QQRTCD            1              RTN CODE
     C                   MOVEL     QQRTCD        QQLR              1
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      QQCDAT        ##CDAT            8 0
     C*
     C*** RTN CODE IS USED TO USE TERM(LR) OR RETRN. (IF IN AS '1') THEN
     C*** SETON LR WHEN DONE.
     C*
     C                   TIME                    DSDTTM
      * Begin Change 10/14/03 *****************************************
     C*********  DSDATE    MULT 10000.01  XXDT    60       YY/MM/DD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
      * End   Change 10/14/03 *****************************************
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
      *================================================================
      *
      *** Process all Docs for the Batch, Chg status of any NON-Indexed
      *** documents.
      *
      * * * CREATE DOCUMENT NOTE RECORD
      *
     C                   EXSR      DNOTES
      *
      *================================================================
     C*
     C*
     C     TERM          TAG
     C     QQLR          CABEQ     '1'           EXIT
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
      *===================================================================
      ***   DNOTES   -     CREATE DOCUMENT  NOTE                     *****
      *===================================================================
     C*
     C     DNOTES        BEGSR
     C*
     C                   TIME                    D2DTTM
     C                   MOVE      D2DATE        XCURYR
     C                   MOVEL     D2DATE        XCURMD
     C*
     C                   Z-ADD     *HIVAL        XXASQ#
     C     DNKY1         SETGT     PDN01
     C     DNKY2         READPE    PDN01                                  60
     C     *IN60         IFEQ      *OFF
     C     1             ADD       DNASQ#        XXASQ#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXASQ#
     C                   ENDIF
     C*
     C                   MOVEL     QQDCID        DNDCID
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*** BEGIN CHG *** 03/06/08 **************************************
     C**********           Z-ADD##CDAT    DNCDAT
     C                   Z-ADD     ##CDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   CALL      'SRCVTDAT'    DTYYM2
     C                   ELSE
     C                   CLEAR                   SRDTO
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   ENDIF
     C                   MOVEL     SRDTO         DNCDAT
     C*** END   CHG *** 03/06/08 **************************************
     C*
     C*******************  MOVE XCURDT    DNCDAT
     C*
     C                   MOVEL     QQPNAM        DNPNAM                         12/31/91 CHG
     C*
     C                   MOVEL     XCURDT        DNTRDT
     C                   MOVE      D2TIME        DNTRTM
     C*** SECURITY VALUES NOT CURRENTLY USED ******
     C                   MOVE      *ZEROS        DNSECL
     C                   MOVEL     *BLANKS       DNSBYL
     C*** PROTECT THIS RCD, AS IT IS A HISTORY RECORD - "H" - HISTORY
     C***
     C                   MOVEL     QQPRFL        DNPRFL
     C******************** MOVEL'H'       DNPRFL
     C*
     C                   MOVEL     *BLANKS       DNSTAT
     C                   MOVEL     Q1USER        DNIUSR
     C                   MOVEL     Q1PGM         DNIPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        DNITIM
      * Begin Change 10/14/03 *****************************************
     C******     DSDATE    MULT 10000.01  DNIDT
     C                   MOVE      XXDT          DNIDT
      * End   Change 10/14/03 *****************************************
     C*
     C                   MOVEL     *BLANKS       DNLUSR
     C                   MOVEL     *BLANKS       DNLPGM
     C                   MOVE      *ZEROS        DNLTIM
     C                   MOVE      *ZEROS        DNLDT
     C*
     C**** FORMAT THE COMMENT ****
     C*
     C                   MOVEL     *BLANKS       XXA75            75
     C                   MOVEL     QQCM75        DNCM75
     C*
     C                   WRITE     PDN01
     C***  ADD 05/15/03 *************************************
     C** PUT DOC NOTES IN FOR INDEXED DARTS
     C*
     C*
     C                   EXSR      ARNOT
     C*** END ADD 05/15/03 ***********************************
     C*
     ***********************************************************
     C     ENDBN         ENDSR
     ***********************************************************
     C***  ADD 05/15/03 *************************************
      *===================================================================
      ***  ARNOT     -     CREATE DOCUMENT  NOTE DARTS               *****
      *===================================================================
     C*
     C     ARNOT         BEGSR
     C*** DETERMINE IF THERE ARE ANY INDEXED ITEMS TO PROCESS
     C     DOK           CHAIN     PDO01                              67
     C     *IN67         DOWEQ     *OFF
     C*
     C     CMK           CHAIN     PCM01                              68
     C*
     C**  BEG ADD *****  09/09/11 **********************************
     C     *IN68         IFEQ      *OFF
     C                   MOVE      'Y'           CMIQUE
     C                   MOVE      Q1PGM         CMPGM
     C                   MOVE      Q1USER        CMUSER
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C                   MOVE      XXDT          CMDT
     C*
     C                   MOVE      DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C                   ENDIF
     C**  END ADD *****  09/09/11 **********************************
     C                   TIME                    D2DTTM
     C                   MOVE      D2DATE        XCURYR
     C                   MOVEL     D2DATE        XCURMD
     C*
     C**  BEG DEL *****  09/09/11 **********************************
     C***  SET AR
     ***
     C** GET NEXT AVAIL ARSEQ#,
     C*******
     C*******              MOVE *HIVAL    $$ASQ#
     C*******    ARK$$     SETGTPAR01
     C*******    ARKY      REDPEPAR01                    62
     C*******
     C*******    *IN62     IFEQ *OFF
     C*******              ELSE
     C*******              MOVE *ZERO     ARASQ#
     C*******              ENDIF
     C*******
     C*******
     ********
     ********
     ********
     C*******              MOVELDODKEY    ARKEY
     C*******              MOVE DODSQ#    ARSEQ#
     C*********************MOVELQQDCID    DNDCID
     C********             MOVELCMCSID    ARCSID
     C******************   MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELCMPNAM    ARPNAM           12/31/91 CHG
     C********             MOVEL*BLANKS   ARCM75
     C********             ADD  1         ARASQ#
     C*******************  MOVE ARASQ#    ARASQ#
      *** Begin Change 10/14/03 ***************************************
     C******               Z-ADD##CDAT    ARCDAT
     C********             MOVE XXDT      ARCDAT
      *** End   Change 10/14/03 ***************************************
     C********
     C*******************  MOVE XCURDT    DNCDAT
     C******
     C*******************  MOVELQQPNAM    ARPNAM           12/31/91 CHG
     C********
     C********             MOVELXCURDT    ARTRDT
     C********             MOVE D2TIME    ARTRTM
     C*** SECURITY VALUES NOT CURRENTLY USED ******
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C***********          MOVE *ZEROS    ARSECL
     C*******************  MOVEL*BLANKS   ARSBYL
     C*** PROTECT THIS RCD, AS IT IS A HISTORY RECORD - "H" - HISTORY
     C***
     C*******************  MOVELQQPRFL    ARPRFL
     C******************** MOVEL'H'       DNPRFL
     C********
     C******************   MOVEL*BLANKS   ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1USER    ARIUSR
     C********             MOVELQ1PGM     ARPGM
     C********             TIME           DSDTTM
     C********             MOVE DSTIME    ARTIME
      *** Begin Change 10/14/03 ***************************************
     C********   DSDATE    MULT 10000.01  ARIDT
     C********   DSDATE    MULT 10000.01  ARDT
     C********             MOVE XXDT      ARIDT
     C********             MOVE XXDT      ARDT
      *** End   Change 10/14/03 ***************************************
     C*
     C**** FORMAT THE COMMENT ****
     C********
     C********             MOVEL*BLANKS   XXA75  75
     C********             MOVELQQCM75    ARCM75
     C********
     C********             WRITEPAR01
     C**  END DEL *****  09/09/11 **********************************
     C**  BEG ADD *****  09/09/11 **********************************
     C                   MOVEL     DODKEY        Q#KEY
     C                   MOVE      DODSQ#        Q#SEQ#
     C*** Begin Chg *** 07/11/19 **********************************
     C**********         MOVEL     *BLANKS       Q#CM75
     C                   MOVEL(p)  DNcm75        Q#CM75
     C*** End   Chg *** 07/11/19 **********************************
     C                   MOVEL     CMPNAM        Q#PNAM                         12/31/91 CHG
     C                   MOVEL     CMSECL        Q#SECL
     C                   MOVEL     Q1USER        Q#USER
     C                   MOVEL     Q1PGM         Q#PGM
     C                   MOVEL     CMRMCD        Q#RMCD
     C                   MOVE      *BLANKS       Q#ASQ#
     C                   MOVEL     CMCSID        Q#CSID
     C                   MOVE      XXDT          Q#CDAT
     C*
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#SEQ#            5
     C                   PARM                    Q#CM75           75
     C                   PARM                    Q#PNAM           10
     C                   PARM                    Q#SECL            3
     C                   PARM                    Q#USER           10
     C                   PARM                    Q#PGM            10
     C                   PARM                    Q#RMCD            5
     C                   PARM      *BLANKS       Q#ASQ#            5
     C                   PARM                    Q#CSID            2
     C                   PARM                    Q#CDAT            7
     C**  END ADD *****  09/09/11 **********************************
     C     DOK           READE     PDO01                                  67
     C                   ENDDO                                                  *IN67
     C*
     ***********************************************************
     C     ENDAR         ENDSR
     ***********************************************************
     C*** END ADD 05/15/03 ***********************************
