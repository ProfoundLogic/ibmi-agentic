     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*TITLE IS02DLR - UPDAT WORKFILE FOR OBJECT LOCATIONS            *
     H* DESCRIPTION:                                                  *
     F* UPDAT A WORKFILE WITH OBJECT LOCATIONS FOR ALL DARTS OBJECTS  *
     F* BASED ON LAST MAINT. DATE
     H*****************************************************************
     H*  CREATED: 01/17/05                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
    H*                                                                *
     H*  10/17/11 C:279 Project: SY195
     H*                 Modify program to update UNC path field and
     H*                 IFS flag.
     H*
    H* 11/02/10  C:183 Project: SY195                                 *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to*
    H*                 get the root folder name.                      *
    H*                                                                *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   60    GENERAL CHAIN                                         *
     H*****************************************************************
     FSDCMLE    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FSDCMLE1   IF   E           K DISK
     FSMMDL4    IF   E           K DISK
     FSMDLL2    IF   E           K DISK
     FSMOLL1    IF   E           K DISK
     F                                     RENAME(POL01:OLRCD)
     FIXOLL     UF A E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  SISFLD                 1     40
     D  OLCOMP                 1      2
     D  OLPREF                 3      6
     D  OLPOL#                 7     15
     D  OLBRCH                16     17
     D  OLCLM#                18     27
     D*
      *** Beg chg ***** 11/02/10 ***********************************************
     D********      'QDLS\'               C         QDLS
     D RPGM            C                   CONST('RTVDOCROOT')
      *** End chg ***** 11/02/10 ***********************************************
     D QOPT            C                   CONST('QOPT\')
     D IFS             C                   CONST('OPTMLB01\')

     I*

     C     *LIKE         DEFINE    CMKEY         XXKEY
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    MDOID         XXOID
     C     *LIKE         DEFINE    MDOFLR        XXOFLR
     C*
     C*  DOC LOCATION FILE
     C     DLKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*  DOC LOCATION FILE
     C     OLKY1         KLIST
     C                   KFLD                    MDKEY
     C                   KFLD                    MDSEQ#
     C***
     C*  REMARK FILE KEY LIST
     C     CMK           KLIST
     C                   KFLD                    DAT               7 0
     C***
     C*  WORKFILE
     C     WKKY          KLIST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXSEQ#
     C                   KFLD                    XXOID
     C                   KFLD                    XXOFLR
     C*
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
      ** Date check/conversion parameters
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *ENTRY        PLIST
     C                   PARM                    INDAT             6
     C*
     C*** CONVERT INDATE TO USE FOR GETTING RECORDS TO PROCESS.
     C***
     C*
      *
     C                   MOVE      INDAT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DAT
      *
     C*
      *** Beg add ***** 11/02/10 ***********************************************
      * Call the Root determination program
     C                   CALL      RPGM
     C                   PARM                    ROOT1            10
     C     ROOT1         CAT       '\':0         ROOT             10
      *** End add ***** 11/02/10 ***********************************************
     C*
     C*
     C*** Read DARTS Remark file
     C     CMK           SETLL     PCM01
     C                   READ      PCM01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C     DAT           ANDLE     CMDT
     C*
     C*** INC. RECORD IF A DOC EXISTS
     C     CMDCID        IFNE      *BLANKS
     C     CMFL01        ANDNE     'Y'
     C     CMIDOC        ORNE      *BLANKS                                        * NON OV DOCS
     C*
     C*** PREPARE OUT FIELDS
     C*
     C                   MOVEL     CMKEY         OLKEY
     C                   MOVE      CMSEQ#        OLSEQ#
     C                   MOVEL     CMKEY         XXKEY
     C                   MOVE      CMSEQ#        XXSEQ#
      *
     C                   MOVEL     *BLANKS       OLOID
     C                   MOVEL     *BLANKS       OLOFLR
     C                   MOVEL     *BLANKS       XXOID
     C                   MOVEL     *BLANKS       XXOFLR
     C***
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
     C*** WRITE RECORDS
     C**** CHAIN TO THE WORKFILE. IF RECORD ALREADY EXISTS, THEN JUST UPDATE,
     C**** ELSE WRITE A NEW RECORD.
     C*
     C*** DETERMINE IF THIS WILL BE UPDATE OR WRITE
     C     WKKY          CHAIN     POL01                              66
     C*
     C     *IN66         IFEQ      *OFF
     C                   UPDATE    POL01
     C                   ELSE
     C                   WRITE     POL01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** Read next DARTS Remark Record
     C                   READ      PCM01                                  50
     C                   ENDDO
     C*
     C***** NOW PROCESS THE MD FILE
     C*
     C*** Read DARTS Remark file
     C     CMK           SETLL     PMD01
     C                   READ      PMD01                                  50
     C     *IN50         DOWEQ     '0'
     C     DAT           ANDLE     MDLDT
     C*
     C*
     C*** PREPARE OUT FIELDS
     C*
     C                   MOVEL     MDKEY         OLKEY
     C                   MOVEL     MDKEY         XXKEY
     C                   MOVE      MDSEQ#        OLSEQ#
     C                   MOVE      MDSEQ#        XXSEQ#
     C                   MOVEL     MDOID         OLOID
     C                   MOVEL     MDOID         XXOID
     C                   MOVEL     MDOFLR        OLOFLR
     C                   MOVEL     MDOFLR        XXOFLR
      *
     C                   MOVEL     MDOTYP        OLOTYP
     C*
     C                   MOVEL     MDRMCD        OLRMCD
     C*
     C                   MOVEL     *BLANKS       OLDESC
     C                   MOVEL     MDODSC        OLDESC
     C*** ADD 12/20/04 ******************************************
     C*** USE THE MAJOR REMARK DESCRIPTION INSTEAD OF THE ATTACHMENT DESC.
     C     OLKY1         CHAIN     PCM02                              93
     C     *IN93         IFEQ      *OFF
     C                   MOVEL     CMCOMM        OLDESC
     C                   ELSE
     C                   MOVEL     MDODSC        OLDESC
     C                   ENDIF
     C*** END ADD 12/20/04 ************************************
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
     C*** WRITE RECORDS
     C*
     C*** DETERMINE IF THIS WILL BE UPDATE OR WRITE
     C     WKKY          CHAIN     POL01                              66
     C     *IN66         IFEQ      *OFF
     C                   UPDATE    POL01
     C                   ELSE
     C                   WRITE     POL01
     C                   ENDIF
     C*
     C*
     C*
     C                   READ      PMD01                                  50
     C                   ENDDO                                                           *IN50
     C*
     C*
     C*
     C*** END PROGRAM
     C*
     C                   MOVE      '1'           *INLR
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
     C     CMUNCN        IFNE      ' '
     C                   MOVEL(P)  CMUNCN        XXPATH          132
     C                   ELSE
     C*** End Add ***** 10/17/11 ***********************************************
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******    QDLS      CAT  CMFOLD:0  XXPATH132 P
     C     ROOT          CAT(P)    CMFOLD:0      XXPATH          132
      *** End chg ***** 11/02/10 ***********************************************
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
     C     MDUNCP        IFNE      ' '
     C                   MOVEL(P)  MDUNCP        XXPATH          132
     C                   ELSE
     C*** End Add ***** 10/17/11 ***********************************************
      *** Beg chg ***** 11/02/10 ***********************************************
     C******     QDLS      CAT  MDOFLR:0  XXPATH132 P
     C     ROOT          CAT(P)    MDOFLR:0      XXPATH          132
      *** End chg ***** 11/02/10 ***********************************************
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
     C*
     C                   ENDSR
     C**************************************************************************
