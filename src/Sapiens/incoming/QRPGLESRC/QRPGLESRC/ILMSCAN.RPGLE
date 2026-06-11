     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *  Description:                                                   *
      *
      *  This API will scan a single document.
      *  It is called by Delta System software as an api to our scan
      *  application.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  Parms to initiate scan.
      *
      *  SEND BACK       :  DO record, return code.
      *  - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    NONE                                                         *
      *                                                                 *
      *  Created:   4/24/02  C:02  Project:                             *
      *                                                                 *
      * *****************************************************************
     H** REVISIONS:
      *                                                               *
      *   10/17/11 C:279 Project: SY195                               *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
      *  06/20/02  CHANGED TO USE A DS AS THE ENTRY PARM 1PARM 1024 LNGTH
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
     F*
     FIMDOL1    IF   E           K DISK
      *
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM0A)
     F** ADD 10/05/02 ********************************************
     FILMLOG    O    E             DISK
     F*** END ADD 10/05/02 ***************************************

     D ER              S             79    DIM(1) CTDATA PERRCD(1)
      *
     D QQFILL          DS
     D  XXFIL1                 1    256
     D  XXFIL2               257    340
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D DODS          E DS                  EXTNAME(IMDOP)
     D  DORC1                  1    256
     D  DORC2                257    275
     D DORCD           DS
     D  DORC3                  1    256
     D  DORC4                257    275
     D SCCOMM          DS
     D  SCCOM1                 1    256
     D  SCCOM2               257    275
     D                 DS
     D  ##DRKY                 1     30
     D  QQKY1                  1     26
     D  QQSQ                  27     30
     D*** BEGIN CHANGES *** USE DS AS PARM *** 06/20/02 ******************
     D IMGPRM          DS
     D  QQRTCD                 1      2
     D  QQEMSG                 3     81
     D  $#DOR1                82    337
     D  $#DOR2               338    356
     D  QQDRKY               357    386
     D  QQDCID               387    398
     D  QQOUTQ               399    408
     D  QQPATH               409    468
     D  QQCPTH               469    528
     D  QQDCTP               529    536
     D  QQFIL1               537    792
     D  QQFIL2               793   1024
     D****** END CHANGES 06/20/02 *** USE DS FOR PARM ******************
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
      ***** END ADD ***** 10/17/11 ***********************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C*
     C*   KEY LIST FOR
     C*
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     C*
     C     CMKY$         KLIST
     C                   KFLD                    Q#KEY
     C                   KFLD                    ##SEQ#
      *
     C***        *LIKE     DEFN XXXX      XXXX
     C*
      * ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C*** BEGIN CHANGES *** USE DS AS PARM *** 06/20/02 ******************
     C***                  PARM           QQRTCD  2        01
     C***                  PARM           QQEMSG 79        02
     C***                  PARM           DORCD            03
     C***                  PARM           QQDRKY 30        04
     C***                  PARM           QQDCID 12        05
     C***                  PARM           QQOUTQ 10        06
     C***                  PARM           QQPATH 60        07
     C***                  PARM           QQCPTH 60        08
     C***                  PARM           QQDCTP  8        09
     C***                  PARM           QQFILL           10
     C*
     C                   PARM                    IMGPRM
     C*
     C                   MOVEL     $#DOR1        DORC3
     C                   MOVEL     $#DOR2        DORC4
     C****** END CHANGES 06/20/02 *** USE DS FOR PARM ******************
      *-------------------------------------------------------------------
     C***        Q1PRMS    IFGT 15
      *-------------------------------------------------------------------
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   MOVEL     QQDRKY        ##DRKY
     C                   MOVEL     QQDRKY        ##KEY            40
      *
      ***** BEG ADD ***** 10/17/11 ***********************************
      * GET ROOT
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
      ***** END ADD ***** 10/17/11 ***********************************
      * GET TODAY'S DATE
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              8 0
     C*
     C*
     C*** 10/05/02 *** ADD LOGGING ****************************
     C*** GET LOGGING DATA AREA TO SEE IF LOGGING SHOULD BE DONE
     C     *DTAARA       DEFINE                  ILMLG             1
     C                   IN        *DTAARA                              83      ERROR IF 83
     C     *IN83         IFEQ      *OFF
     C     ILMLG         ANDNE     *BLANK
     *** LOGGING IS TO BE PERFORMED
     C                   MOVEL     'Y'           LOGIT             1
     C                   MOVEL     Q1PGM         #LCPGM
     C                   MOVEL     'IN'          #LSCOD
     C                   MOVE      XXDT          #LDT
     C                   MOVE      DSTIME        #LTM
     C                   MOVEL     Q1JOB         #JOB
     C                   MOVE      Q1JOBN        #JOBN
     C                   MOVEL     Q1USER        #USER
     C                   MOVEL     *BLANKS       #TEXT
     C*
     C*
     C*
     C                   WRITE     RLOG
     C*
     C                   ENDIF
     C***
     C*** END ADD 10/05/02 ************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCCDAT           80
     C*
      *
      * VERIFY THE DOC TYPE PASSED IN  -  SCREEN SELECT DOCTYP IF INVALID
      *
     C     DOCTTG        TAG
      *
     C                   CALL      'IS715R'
     C                   PARM                    QQDCTP                         DOC TYPE
     C                   PARM                    QQRMCD            5            RMK CODE
     C                   PARM                    QQDLIM            3            DELIMIETER
     C                   PARM                    QQDDSC           40            DESC
     C                   PARM                    QQDQID           10            DTYP QID
     C                   PARM                    QQ#DIB            5            #DCS IN BTCH
     C                   PARM                    QQSECV            3            SEC VALUE
     C                   PARM                    QQIDXP           10            INDX PGM
     C                   PARM                    QQBSTS            2            BTCH STATUS
     C                   PARM      *BLANKS       Q715RC            1            RTN CODE
      *
     C     Q715RC        IFNE      *BLANKS
     C*** INVALID DOCTYPE WAS FOUND
     C*** DISPLAY A SELECTION SCREEN FOR DOCTYPES
     C*** FOR USER TO SELECT DOCTYPE
     C                   CALL      'IS100R'
     C                   PARM                    QQDCTP
     C                   PARM                    QQDDSC
     C                   PARM                    Q100RC            1
     C     Q100RC        IFNE      *BLANK
     C*** nothing selected, or f3/12 pressed on Select of Doc Type
     C                   MOVE      '01'          QQRTCD
     C                   MOVEA     ER(01)        QQEMSG
     C                   GOTO      TERM
     C*
     C                   ELSE
     C                   GOTO      DOCTTG
      *
     C                   ENDIF
      *
     C*
     C                   ENDIF
      *
      * WRITE DART RECORD
      *
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANK        SCIQUE           10                HIDDEN
     C                   MOVEL     *BLANK        SCIPTH           10                HIDDEN
     C                   MOVE      *ZEROS        SCPIDX            5 0              HIDDEN
     C                   MOVEL     *BLANK        SCAPID            2                HIDDEN
     C                   MOVEL     *BLANK        SCIDOC           12                HIDDEN
     C                   MOVEL     *BLANK        SCIMGF            1                HIDDEN
     C                   MOVEL     *BLANK        SCQPTY            1                HIDDEN
     C                   MOVEL     QQRMCD        SCDCTP            5                HIDDEN
     C                   MOVEL     *BLANK        SCFL05            1                HIDDEN
     C*
     C*
     C                   MOVEL     *BLANKS       QSRTCD
      *
     C                   MOVEL     '***IMAGE'    SCDCID
     C                   MOVE      '*** '        SCDCID           12            SET FIELDS
     C                   MOVE      'Y'           SCIMGF
     C*
     C                   EXSR      WCOM
     C                   MOVEL     Q#COMM        SCCOM1
     C                   MOVE      Q#COM2        SCCOM2
     C*** 10/05/02 *** ADD LOGGING ****************************
     C     LOGIT         IFEQ      'Y'
     C*
     C                   MOVEL     Q1PGM         #LCPGM
     C                   MOVEL     'SB'          #LSCOD
     C                   MOVE      XXDT          #LDT
     C                   MOVE      DSTIME        #LTM
     C                   MOVEL     Q1JOB         #JOB
     C                   MOVE      Q1JOBN        #JOBN
     C                   MOVEL     Q1USER        #USER
     C                   MOVEL     *BLANKS       #TEXT
     C*
     C                   MOVE      ##SEQ#        $$SQ1             5
     C     ##KEY         CAT(P)    $$SQ1:0       #TEXT
     C     #TEXT         CAT(P)    QSRTCD:0      #TEXT
     C     #TEXT         CAT(P)    SCIDOC:0      #TEXT
     C     #TEXT         CAT(P)    ZZFOLD:0      #TEXT
     C*
     C*
     C                   WRITE     RLOG
     C*
     C                   ENDIF
     C***
     C*** END ADD 10/05/02 ************************************
      *
      * CALL SCIMGS
     C*
     C                   CALL      'SCIMGS'                             60
     C                   PARM                    SCIDOC           12
     C                   PARM                    SCCOMM
     C                   PARM      Q1USER        SCDUSR           10
     C                   PARM      ##KEY         QSKEY            40
     C                   PARM      QQDQID        SCIQUE
     C                   PARM                    SCAPID
     C                   PARM                    SCIPTH
     C                   PARM                    SCPIDX
     C                   PARM                    SCQPTY
     C                   PARM                    SCDCTP
     C                   PARM                    ##SEQ#
     C                   PARM                    ZZFOLD           40
     C                   PARM                    ZZERRM           60
     C                   PARM                    QSRTCD            6
     C*
     C*** 10/05/02 *** ADD LOGGING ****************************
     C     LOGIT         IFEQ      'Y'
     C*
     C                   MOVEL     Q1PGM         #LCPGM
     C                   MOVEL     'SC'          #LSCOD
     C                   MOVE      XXDT          #LDT
     C                   MOVE      DSTIME        #LTM
     C                   MOVEL     Q1JOB         #JOB
     C                   MOVE      Q1JOBN        #JOBN
     C                   MOVEL     Q1USER        #USER
     C                   MOVEL     *BLANKS       #TEXT
     C*
     C                   MOVE      ##SEQ#        $$SQ1
     C     ##KEY         CAT(P)    $$SQ1:0       #TEXT
     C     #TEXT         CAT(P)    QSRTCD:0      #TEXT
     C     #TEXT         CAT(P)    SCIDOC:0      #TEXT
     C     #TEXT         CAT(P)    ZZFOLD:0      #TEXT
     C*
     C*
     C                   WRITE     RLOG
     C*
     C                   ENDIF
     C***
     C*** END ADD 10/05/02 ************************************
     C     QSRTCD        IFEQ      '     0'                                     VALID SCAN
     C     QSRTCD        OREQ      '000000'                                     VALID SCAN
     C     QSRTCD        OREQ      '      '                                     VALID SCAN
      *
      *** UPDATE THE DARTS RECORD WITH THE IMAGE VALUES FROM SUCCESSFUL
      *** SCAN.
      *
     C                   EXSR      UPDSCN
      *
     C                   ELSE
     C*
      *
     C                   EXSR      DLTSCN
      *
     C                   MOVEL     ZZERRM        QQEMSG
     C*** 10/05/02 *** ADD ******************88
     C************ INC. ERRCODE
     C                   MOVEL     QSRTCD        QQRTCD
     C***  END ADD 10/05/02 **********************
      *
     C                   END
     C*
     C     TERM          TAG
     C*** 10/05/02 *** ADD LOGGING ****************************
     C     LOGIT         IFEQ      'Y'
     C*
     C                   MOVEL     Q1PGM         #LCPGM
     C                   MOVEL     'OT'          #LSCOD
     C                   MOVE      XXDT          #LDT
     C                   MOVE      DSTIME        #LTM
     C                   MOVEL     Q1JOB         #JOB
     C                   MOVE      Q1JOBN        #JOBN
     C                   MOVEL     Q1USER        #USER
     C                   MOVEL     *BLANKS       #TEXT
     C*
     C*
     C*
     C                   WRITE     RLOG
     C*
     C                   ENDIF
     C***
     C*** END ADD 10/05/02 ************************************
     C                   MOVE      *ON           *INLR
      *
      *----------------------------------------------------------------
     C     WCOM          BEGSR
      *----------------------------------------------------------------
      *
     C*          ------------------------------------------------------
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  ##DRKY        Q#KEY            40
     C                   MOVEL(P)  QQRMCD        Q#RMCD            5
     C                   MOVEL(P)  SCDCID        Q#DCID           12
     C                   MOVEL     QQDDSC        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVE      *BLANK        Q#FL01            1
     C                   MOVEL     *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  SCCDAT        Q#CDAT            8
     C                   MOVEL(P)  'DELTA   '    Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL(P)  SCDUSR        Q#USER           10
     C                   ELSE                                                   DUSR = blanks
     C                   MOVEL     Q1USER        Q#USER
     C                   ENDIF                                                  DUSR = blanks
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVE      'Y'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  SCAPID        Q#APID            2
     C                   MOVEL(P)  SCIDOC        Q#IDOC           12
     C                   MOVEL(P)  SCIQUE        Q#IQUE           10
     C                   MOVEL(P)  SCIMGF        Q#IMGF            1
     C                   MOVEL(P)  SCIPTH        Q#IPTH           10
     C                   MOVE      SCPIDX        Q#PIDX            5
     C                   MOVEL(P)  SCQPTY        Q#QPTY            1
     C                   MOVEL(P)  SCDCTP        Q#DCTP            5
     C                   MOVEL     'XX'          Q#CSID            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   MOVE      Q#SEQ#        ##SEQ#            5 0
      *
     C     EWCOM         ENDSR
      *====================================================================*
      * * *   UPDSCN SUBROUTINE - UPDATE CM RCD FROM SCN    U P D S C N    *
      *====================================================================*
     C*
     C     UPDSCN        BEGSR
     C*
     C     CMKY$         CHAIN     PCM0A                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     SCIDOC        CMIDOC
     C                   MOVEL     SCIDOC        CMDCID
     C                   MOVE      *BLANKS       CMFOLD
     C                   MOVEL     ZZFOLD        CMFOLD
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVE      *BLANKS       CMIFSP
     C                   MOVE      *BLANKS       CMUNCN
     C                   MOVEL     ENV           CMIFSP
      *
     C     CMFOLD        IFNE      ' '
     C     CMDCID        ORNE      ' '
     C                   MOVEL     '/'           CMUNCN
     C     CMUNCN        CAT       ROOT:0        CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMFOLD:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMDCID:0      CMUNCN
     C                   ELSE
     C                   MOVE      *BLANKS       CMUNCN
     C                   ENDIF                                                  CMFOLD IFNE ' '
      ***** END ADD ***** 10/17/11 ***********************************
     C                   UPDATE    PCM0A
     C*
     C                   MOVEL     CMIDOC        QQDCID
     C                   MOVEL     CMFOLD        QQPATH
      *
     C                   ENDIF
     C*
      * **  GET DO INFO FOR RETURN TO CALLER
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
     C*** FIELD VALS SHOULD BE MAPPED TO DORCD FIELD (275A)
     C                   CLEAR                   DORCD
     I*** BEGIN CHANGES *** USE DS AS PARM *** 06/20/02 ******************
     C                   MOVEL     DORC1         $#DOR1
     C                   MOVE      DORC2         $#DOR2
     C****** END CHANGES 06/20/02 *** USE DS FOR PARM ******************
     C                   MOVE      DORC1         DORC3
     C                   MOVE      DORC2         DORC4
     C                   ENDIF
      * * * UPDATE THE STATUS OF THE RCDS AS COMPLETE (INDEXED)
     C                   CALL      'IS722R'
     C                   PARM      QQDCID        $$DCID           12
     C                   PARM      'CM'          $$NSTS            2
     C                   PARM      'SC'          $$OSTS            2
     C                   PARM      Q1PGM         $$CPGM           10
     C                   PARM      *BLANK        $$RTN             1
     C*** 10/05/02 *** ADD LOGGING ****************************
     C     LOGIT         IFEQ      'Y'
     C*
     C                   MOVEL     Q1PGM         #LCPGM
     C                   MOVEL     'SU'          #LSCOD
     C                   MOVE      XXDT          #LDT
     C                   MOVE      DSTIME        #LTM
     C                   MOVEL     Q1JOB         #JOB
     C                   MOVE      Q1JOBN        #JOBN
     C                   MOVEL     Q1USER        #USER
     C                   MOVEL     *BLANKS       #TEXT
     C*
     C                   MOVE      DODSQ#        $$SQ              5
     C                   MOVE      ##SEQ#        $$SQ1
     C     DODCID        CAT(P)    QQDCID:0      #TEXT
     C     #TEXT         CAT(P)    DODKEY:0      #TEXT
     C     #TEXT         CAT(P)    $$SQ:0        #TEXT
     C     #TEXT         CAT(P)    Q#KEY:0       #TEXT
     C     #TEXT         CAT(P)    $$SQ1:0       #TEXT
     C*
     C*
     C                   WRITE     RLOG
     C*
     C                   ENDIF
     C***
     C*** END ADD 10/05/02 ************************************
     C*
     C     EUPDSC        ENDSR
      *====================================================================*
      * * *   DLTSCN SUBROUTINE - DELETE CM RCD FROM SCN    D L T S C N    *
      *====================================================================*
     C*
     C     DLTSCN        BEGSR
     C*
     C     CMKY$         DELETE    PCM0A                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*** VALID DELETE *********** ELSE ?????
     C*
     C*
     C                   ENDIF
     C*** 10/05/02 *** ADD LOGGING ****************************
     *** CLEAR THE DO RECORD ON AN ERROR SCAN !!!!!!!!!
     C                   CLEAR                   DORCD
     C                   MOVEL     DORC1         $#DOR1
     C                   MOVE      DORC2         $#DOR2
     C                   MOVE      DORC1         DORC3
     C                   MOVE      DORC2         DORC4
     C*
     C     LOGIT         IFEQ      'Y'
     C*
     C                   MOVEL     Q1PGM         #LCPGM
     C                   MOVEL     'SD'          #LSCOD
     C                   MOVE      XXDT          #LDT
     C                   MOVE      DSTIME        #LTM
     C                   MOVEL     Q1JOB         #JOB
     C                   MOVE      Q1JOBN        #JOBN
     C                   MOVEL     Q1USER        #USER
     C                   MOVEL     *BLANKS       #TEXT
     C*
     C                   MOVE      *BLANK        $$SQ              5
     C                   MOVE      ##SEQ#        $$SQ1             5
     C     DODCID        CAT(P)    QQDCID:0      #TEXT
     C     #TEXT         CAT(P)    DODKEY:0      #TEXT
     C     #TEXT         CAT(P)    $$SQ1:0       #TEXT
     C     #TEXT         CAT(P)    Q#KEY:0       #TEXT
     C     #TEXT         CAT(P)    $$SQ1:0       #TEXT
     C*
     C*
     C                   WRITE     RLOG
     C*
     C                   ENDIF
     C***
     C*** END ADD 10/05/02 ************************************
     C*
     C     EDLTSC        ENDSR
** ERM error message text
Invalid Doctype input, valid Doctype not selected.
