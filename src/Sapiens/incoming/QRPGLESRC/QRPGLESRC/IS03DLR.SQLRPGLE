     H*****************************************************************
     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H*TITLE IS03DLR - STORED PROCEDURE - GET OBJ LOCATIONS           *
     H* DESCRIPTION:                                                  *
     F*
     H*****************************************************************
     H*  CREATED: 12/19/05                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  10/17/11 C:279 Project: SY195                                *
     H*                 Modify program to update UNC path field.      *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   60    GENERAL CHAIN                                         *
     H*****************************************************************
     FSDCMLE    IF   E           K DISK
     FSMMDL     IF   E           K DISK
     FSMDLL2    IF   E           K DISK
     FSMOLL1    IF   E           K DISK
     F                                     RENAME(POL01:OLRCD)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     DIXOLP          E DS                  EXTNAME(IXOLP) OCCURS(999)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  SISFLD                 1     40
     D  ##COMP                 1      2
     D  ##PREF                 3      6
     D  ##POL#                 7     15
     D  ##BRCH                16     17
     D  ##CLM#                18     27
     D*
     D QDLS            C                   CONST('QDLS\')
     D QOPT            C                   CONST('QOPT\')
     D IFS             C                   CONST('OPTMLB01\')
     Di                s              4  0
     I*
     C*  DOC LOCATION FILE
     C     DLKY          KLIST
     C                   KFLD                    INKY
     C                   KFLD                    CMSEQ#
      *
     C** DART FILE
     C     CMK           KLIST
     C                   KFLD                    INKY
      *
     C*  DOC LOCATION FILE
     C     OLKY1         KLIST
     C                   KFLD                    INKY
     C                   KFLD                    MDSEQ#
      *
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
      ** Date check/conversion parameters
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *ENTRY        PLIST
     C                   PARM                    INKY             40
      *
     C*
     **** SET INDEX TO 0
     C                   EVAL      I=0
     C*
     C*** USE KEY TO "GET" ALL RECORDS THAT MEET CRITERIA
     C*** Read DARTS Remark file
      ****
     C     CMK           CHAIN     PCM01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C*** INC. RECORD IF A DOC EXISTS
     C     CMDCID        IFNE      *BLANKS
     C     CMFL01        ANDNE     'Y'
     C     CMIDOC        ORNE      *BLANKS
     C*
     C**
     C*** INCREMENT INDEX
     C                   EVAL      I=I+1
     C*** SET OCCURANCE FOR DS
     C     I             OCCUR     IXOLP
     C***
     C*** PREPARE OUT FIELDS
     C*
     C                   MOVEL     CMKEY         OLKEY
     C                   MOVE      CMSEQ#        OLSEQ#
     C                   MOVEL     *BLANKS       OLOID
     C                   MOVEL     *BLANKS       OLOFLR
     C     CMIMGF        IFEQ      *BLANKS
     C                   MOVEL     'X'           OLOTYP
     C                   ELSE
     C     CMIMGF        IFEQ      'I'
     C     CMIMGF        OREQ      'Y'
     C                   MOVEL     'I'           OLOTYP
     C                   ELSE
     C     CMIMGF        IFEQ      'F'
     C                   MOVEL     'F'           OLOTYP
     C                   ELSE
     C                   MOVEL     'O'           OLOTYP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     CMRMCD        OLRMCD
     C*
     C                   MOVEL     *BLANKS       OLDESC
     C                   MOVEL     CMCOMM        OLDESC
     C*
     C**** BUILD CREATE DATE OUTFMT IS YYYYMMDD
     C                   MOVEL     CMTRDT        ##TRDT            7 0
     C                   EXSR      BLDDAT
     C*
     C*** BUILD THE SIS FIELDS FOR OUTPUT   IF   THIS IS AN SIS TYPE (CMPGM)
     C                   MOVEL     CMKEY         ##KEY            40
     C                   EXSR      BLDSIS
     C*
     C***
     C*** BUILD THE PATH AND OBJECT FIELDS
     C**** CMFIELDS TO USE FOR THIS.
     C                   EXSR      BLDPTH
     C*** DATA IS IN OCCURANCE DS
     C*
     C                   ENDIF
     C*
     C*** Read next DARTS Remark Record
     C     CMK           READE     PCM01                                  50
     C                   ENDDO
     C*
     C***** NOW PROCESS THE MD FILE
     C*
     C*** Read DARTS Remark file
     C     CMK           CHAIN     PMD01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C*
     C*** INCREMENT INDEX
     C                   EVAL      I=I+1
     C*** SET OCCURANCE FOR DS
     C     I             OCCUR     IXOLP
     C***
     C*** PREPARE OUT FIELDS
     C*
     C                   MOVEL     MDKEY         OLKEY
     C                   MOVE      MDSEQ#        OLSEQ#
     C                   MOVEL     MDOID         OLOID
     C                   MOVEL     MDOFLR        OLOFLR
     C                   MOVEL     MDOTYP        OLOTYP
     C*
     C                   MOVEL     MDRMCD        OLRMCD
     C*
     C                   MOVEL     *BLANKS       OLDESC
     C                   MOVEL     MDODSC        OLDESC
     C*** USE THE MAJOR REMARK DESCRIPTION INSTEAD OF THE ATTACHMENT DESC.
     C     OLKY1         CHAIN     PCM01                              93
     C     *IN93         IFEQ      *OFF
     C                   MOVEL     CMCOMM        OLDESC
     C                   ELSE
     C                   MOVEL     MDODSC        OLDESC
     C                   ENDIF
     C*
     C**** BUILD CREATE DATE OUTFMT IS YYYYMMDD
     C                   MOVEL     MDTRDT        ##TRDT            7 0
     C                   EXSR      BLDDAT
     C*
     C*** BUILD THE SIS FIELDS FOR OUTPUT   IF   THIS IS AN SIS TYPE (CMPGM)
     C                   MOVEL     MDKEY         ##KEY            40
     C                   EXSR      BLDSIS
     C*
     C***
     C*** BUILD THE PATH AND OBJECT FIELDS
     C**** CMFIELDS TO USE FOR THIS.
     C                   EXSR      BLDPT1
     C*** DATA IS IN DS
     C*
     C*
     C*
     C     CMK           READE     PMD01                                  50
     C                   ENDDO                                                           *IN50
     C*
     **----------------------- SQL area ---------------------------------------------------**
     C*
     c/exec sql
     c+   set result sets array :IXOLP   for :i rows
     c/end-exec
     C*
     C*** END PROGRAM
     C*
     C                   MOVE      *ON           *INLR
     C*
     C**************************************************************************
     C     BLDDAT        BEGSR
     C*** BUILD OLCRDT FIELD FROM ##TRDT
      * GET TODAY'S DATE
      *
     C                   Z-ADD     ##TRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         OLCRDT
      *
     C                   ENDSR
     C**************************************************************************
     C**************************************************************************
     C     BLDPTH        BEGSR
     C*** BUILD BOTH THE OLPATH AND THE OLONAM FIELDS
     C*** DETERMINE THE LOCATION TO DECIDE ON HOW TO BUILD PATH
     C*
     C*** CM RCDS, 1) OPTICAL FLAG, IFNOT O THEN USE FOLD AND DCID OR IDOC
     C***   \QDLS /     IF O THEN GET DL RECORD, USE THOSE VALUES, CHECK FOR
     C***    VIRTUAL OPTICAL TO DETERMINE QOPT OR THE IFS.
     C*
     C                   MOVEL     *BLANKS       XXPATH
     C*** FILL ORIGINAL OBJECT NAME IN THIS FIELD
     C     CMIMGF        IFEQ      'I'
     C     CMIMGF        OREQ      'F'
     C     CMIMGF        OREQ      'Y'
     C                   MOVEL     CMIDOC        OLONAM
     C                   ELSE
     C                   MOVEL     CMDCID        OLONAM
     C                   ENDIF
     C*
     C     CMFL05        IFNE      'O'
     C*** NOT OPTICAL USE CM FIELDS
     C*** Beg Add ***** 10/17/11 ***********************************************
     C                   IF        CMUNCN <> *BLANKS
     C                   MOVEL     CMUNCN        XXPATH          132
     C                   ELSE
     C*** End Add ***** 10/17/11 ***********************************************
     C     QDLS          CAT(P)    CMFOLD:0      XXPATH          132
     C     XXPATH        CAT(P)    '\':0         XXPATH          132
     C     CMIMGF        IFEQ      'I'
     C     CMIMGF        OREQ      'F'
     C     CMIMGF        OREQ      'Y'
     C     XXPATH        CAT(P)    CMIDOC:0      XXPATH
     C                   ELSE
     C     XXPATH        CAT(P)    CMDCID:0      XXPATH
     C                   ENDIF
     C*** Beg Add ***** 10/17/11 ***********************************************
     C                   ENDIF
     C*** End Add ***** 10/17/11 ***********************************************
     C*
     C                   ELSE                                                   CMFL05 <>'O'
     C*** OPTICAL, MUST GET THE DL RECORD, TO GET LOCATION AS WELL AS
     C***  DETERMINE VIRTUAL OPTICAL.
     C*
     C*** GET VIRTUAL OPTICAL FLAG ***
     C     *DTAARA       DEFINE                  VRTOPT            1
     C                   IN        *DTAARA                              83      ERROR IF 83
     C     *IN83         IFEQ      *ON
     C*** ERROR, SO BLANK THE VRTOPT FLAG
     C                   MOVEL     *BLANK        VRTOPT
     C                   ENDIF
     C*** VRTOPT = Y IF USING VRTOPT
     C*
     C     DLKY          CHAIN     PDL01                              61
     C     *IN61         IFEQ      *OFF
     C     VRTOPT        IFNE      'Y'
     C                   MOVEL(P)  QOPT          XXPATH
     C                   ELSE
     C                   MOVEL(P)  IFS           XXPATH
     C                   ENDIF
     C*
     C     XXPATH        CAT(P)    DLVOL1:0      XXPATH
     C     XXPATH        CAT(P)    '\':0         XXPATH
     C     XXPATH        CAT(P)    DLOFLD:0      XXPATH
     C     XXPATH        CAT(P)    '\':0         XXPATH
     C     XXPATH        CAT(P)    DLOSBD:0      XXPATH
     C     XXPATH        CAT(P)    '\':0         XXPATH
     C     XXPATH        CAT(P)    DLDCID:0      XXPATH
     C*
     C                   ELSE
     C                   MOVEL     'ERROR!'      XXPATH
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL     XXPATH        OLPATH
     C*
     C                   ENDSR
     C**************************************************************************
     C**************************************************************************
     C     BLDPT1        BEGSR
     C*** BUILD BOTH THE OLPATH AND THE OLONAM FIELDS
     C*** DETERMINE THE LOCATION TO DECIDE ON HOW TO BUILD PATH
     C*
     C*** MD RCDS, 1) CHAIN TO OL FILE SEE IF ON OPTIAL
     C*
     C                   MOVEL     *BLANKS       XXPATH
     C*** FILL ORIGINAL OBJECT NAME IN THIS FIELD
     C                   MOVEL     MDOID         OLONAM
     C*
     C     OLKY1         CHAIN     OLRCD                              61
     C     *IN61         IFEQ      *ON
     C*** NOT OPTICAL USE MD FIELDS
     C*** Beg Add ***** 10/17/11 ***********************************************
     C                   IF        MDUNCP <> *BLANKS
     C                   MOVEL     MDUNCP        XXPATH          132
     C                   ELSE
     C*** End Add ***** 10/17/11 ***********************************************
     C     QDLS          CAT(P)    MDOFLR:0      XXPATH          132
     C     XXPATH        CAT(P)    '\':0         XXPATH          132
     C     XXPATH        CAT(P)    MDOID:0       XXPATH
     C*** Beg Add ***** 10/17/11 ***********************************************
     C                   ENDIF
     C*** End Add ***** 10/17/11 ***********************************************
     C*
     C                   ELSE                                                   *IN61  <> *ON
     C*** OPTICAL, MUST GET THE OL RECORD, TO GET LOCATION AS WELL AS
     C***  DETERMINE VIRTUAL OPTICAL.
     C*
     C*** GET VIRTUAL OPTICAL FLAG ***
     C                   IN        *DTAARA                              83      ERROR IF 83
     C     *IN83         IFEQ      *ON
     C*** ERROR, SO BLANK THE VRTOPT FLAG
     C                   MOVEL     *BLANK        VRTOPT
     C                   ENDIF
     C*** VRTOPT = Y IF USING VRTOPT
     C*
     C     VRTOPT        IFNE      'Y'
     C                   MOVEL(P)  QOPT          XXPATH
     C                   ELSE
     C                   MOVEL(P)  IFS           XXPATH
     C                   ENDIF
     C*
     C     XXPATH        CAT(P)    OLVOL1:0      XXPATH
     C     XXPATH        CAT(P)    '\':0         XXPATH
     C     XXPATH        CAT(P)    OLOFLD:0      XXPATH
     C     XXPATH        CAT(P)    '\':0         XXPATH
     C     XXPATH        CAT(P)    OLOSBD:0      XXPATH
     C     XXPATH        CAT(P)    '\':0         XXPATH
     C     XXPATH        CAT(P)    OLDCID:0      XXPATH
     C*
     C*
     C                   ENDIF
     C*
     C*
     C                   MOVEL     XXPATH        OLPATH
     C*
     C                   ENDSR
     C**************************************************************************
     C**************************************************************************
     C     BLDSIS        BEGSR
     C*** BUILD THE OLRTYP, OLCOMP, OLPREF, OLPOL#, OLBRCH, OLCLM# (BRCH/CLM IF
     C**                                                    SISCLM TYPE)
     C                   MOVEL     *BLANKS       SISFLD
     C                   MOVEL     ##KEY         SISFLD
     **
     C*** MOVE FIELDS INTO OUTPUT FORMAT
     C                   EVAL      OLCOMP=##COMP
     C                   EVAL      OLPREF=##PREF
     C                   EVAL      OLPOL#=##POL#
     C                   EVAL      OLBRCH=##BRCH
     C                   EVAL      OLCLM#=##CLM#
     C*
     C                   ENDSR
     C**************************************************************************
