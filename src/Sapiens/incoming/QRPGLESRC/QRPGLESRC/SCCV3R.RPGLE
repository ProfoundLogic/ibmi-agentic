     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE SCCV3R - DELETE PROCESS1 - UPDATE DARTS WITH NEW INFO
      *
      **** REMEBER, IF CLIENT HAS VDARTS, MORE WORK MUST BE DONE
      ****  IN THESE PGMS, AS OTHER OBJS. ALREADY EXIST IN FOLDERS.
      ****   THE SELECT PROCESS TO BUILD THE WRK FILE MUST KNOW THIS
      * * *
      ***************************************************************
      * CALLS TO/PARMS
      *
      ***************************************************************
      *  REVISIONS:
      *                                                               *
      *   10/17/11 C:279 Project: SY195                               *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
     H***************************************************************
     FSXCVP     UF   E             DISK
     F*
     FSDCMLE    UF   E           K DISK
     F*
     FSMDLL2    UF   E           K DISK
     F*
     F*

     D*
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * * *  END PROGRAM CONTROL DATA AREA (NON BLANK MEANS END PROGRAM)
      * * *    X#FLG = THIS PROGRAM CONTROL  X#FLG2 = DELETE PROCESS CTL
     D SCCV           UDS
     D  X#FLG                  1      1
     D  X#FLG2                 2      2
     D  X#BLNK                 3    256
     D*
     D                 DS
     D  ERDS                   1     40
     D  ERPGM                  1      8
     D  ERBLK1                 9      9
     D  ERRTCD                10     10
     D  ERBLK2                11     11
     D  ERDESC                12     40
     D                 DS
     D  XXCM75                 1     75
     D  X6A1                   1      6
     D  XXDCID                 7     18
     D  X8A1                  19     26
     D  XXFOLD                27     57
     D  X18A1                 58     75
     D DCID            C                   CONST('DCID:')
     D FOLD            C                   CONST('FOLD:')
     D CM              C                   CONST('NOT FND ON CVT')
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
      ***** END ADD ***** 10/17/11 ***********************************
      *
      * =================================== *
      * * *  KEY LISTS                      *
      * =================================== *

     C*
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
     C*** DARTS RECORD
      *
     C     CMKY          KLIST
     C                   KFLD                    CVKEY
     C                   KFLD                    CVSEQ#
      *
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     DLKY          KLIST
     C                   KFLD                    CVKEY
     C                   KFLD                    CVSEQ#
     C*
      *
      *
     C*
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *
     C*
     C*** ENTRY PARM LIST
     C     *ENTRY        PLIST
     C                   PARM                    QQRTN             1
      *
      ***** BEG ADD ***** 10/17/11 ***********************************
      * GET ROOT
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
      ***** END ADD ***** 10/17/11 ***********************************
      *
     C                   UNLOCK    SCCV
      *
     C     *DTAARA       DEFINE                  SCCV
      *
     C                   Z-ADD     0             COUNT             4 0
      *** READ EACH RECORD SELECTED FOR PURGE.
     C*
     C                   READ      PCV01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     *** CHECK FOR END OF JOB REQUEST (USE DATA AREA FLAG?)
     ***       CLEAR FLAG, END PGM
     C                   IN        SCCV
     C     X#FLG2        IFNE      *BLANK
     C     *LOCK         IN        SCCV
     C                   MOVE      *BLANK        X#FLG2
     C                   OUT       SCCV
     C                   GOTO      TERM
     C                   ENDIF
     ***
     C* BYPASS THE RECORD IF ALREADY PROCESSED... EITHER IT IS "C" CONVERTED
     C*** OR THE ERROR FLAG IS SET IN ORDER TO USE THE RECORD.
     C*
     C     CVPRCS        IFNE      'C'
     C     CVEFLG        ANDNE     'E'
     C                   GOTO      NXTONE
     C                   ENDIF
     C*
     C*** 1) GET CM RCD (KEY/SEQ# NO SELECT/OMIT)
     C*
     C     CMKY          CHAIN(N)  PCM01                              61
     C*
     C*** IF NO HIT, PUT ERROR CODE IN THE PROCESS FLAG FOR LATER RETRIEVE.
     C     *IN61         IFEQ      *ON
     C                   MOVEL     '1'           CVPRCS
     C                   UPDATE    PCV01
     C                   GOTO      NXTONE
     C                   ENDIF
     C***
     C*** 2) CHECK THE CVRCD FOR ERROR (CVEFLG = 'E')
     C***      IF ERROR, THEN CLEAR THE DCID/FLR FIELDS IN CM, AND NO OPTCL
     C***
     C*** 3) IF NO ERROR, MOVE CVNDOC CVNFLD TO CMDCID CMFOLD, SET OPTICAL
     C***     FLAG TO BLANK, IF OPT FLAG WAS "O", MUST ALSO UPDAT/DELET DL
     C***     FILE RECORD.
     C*
     C     CVEFLG        IFEQ      'E'
     C                   MOVEL     *BLANKS       CMDCID
     C                   MOVEL     *BLANKS       CMFOLD
     C                   MOVEL     *BLANKS       CMFL05
     C                   MOVEL     'SCCV3R'      CMPGM
     C                   MOVE      CMSEQ#        XXSEQ#            5
     C**** WRITE AN ADDITIONAL REMARK WITH THE ORIGINAL DCID AND FOLD VALUES
     C                   MOVEL     CVDCID        XXDCID
     C                   MOVEL     CVFOLD        XXFOLD
     C                   MOVEL     DCID          X6A1
     C                   MOVEL     FOLD          X8A1
     C                   MOVEL     CM            X18A1
     C                   MOVEL     XXCM75        QQCM75           75
     C                   MOVEL     CMPNAM        QQPNAM           10
     C                   MOVEL     '000'         QQSECL            3
     C                   MOVEL     Q1USER        QQUSER           10
     C                   MOVEL     CMPGM         QQPGM            10
     C                   MOVEL     CMRMCD        QQRMCD            5
     C                   MOVEL     *ZEROS        QQASQ#            5
     C                   CALL      'DA701R'
     C                   PARM      CMKEY         QQKEY            40
     C                   PARM      XXSEQ#        QQSEQ#            5
     C                   PARM                    QQCM75           75
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQSECL            3
     C                   PARM                    QQUSER           10
     C                   PARM                    QQPGM            10
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQASQ#            5
     C                   ELSE
     C                   ENDIF
     C     CMKY          CHAIN     PCM01                              61
     C     *IN61         IFEQ      *OFF
     C     CVEFLG        IFNE      'E'
     C                   MOVEL     *BLANKS       CMDCID
     C                   MOVEL     *BLANKS       CMFOLD
     C                   MOVEL     CVNDOC        CMDCID
     C                   MOVEL     CVNFLD        CMFOLD
     C                   MOVEL     'O'           CMIMGF
     C                   MOVEL     *BLANKS       CMFL05
     C                   MOVEL     'SCCV3R'      CMPGM
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     *BLANKS       CMIFSP
     C                   MOVEL     *BLANKS       CMUNCN
      *
     C     CMFOLD        IFNE      ' '
     C     CMDCID        ORNE      ' '
     C                   MOVEL     '/'           CMUNCN
     C     CMUNCN        CAT       ROOT:0        CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMFOLD:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMDCID:0      CMUNCN
     C                   MOVEL     ENV           CMIFSP
     C                   ELSE
     C                   MOVE      *BLANKS       CMUNCN
     C                   MOVEL     *BLANKS       CMIFSP
     C                   ENDIF                                                  CMFOLD IFNE ' '
      ***** END ADD ***** 10/17/11 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       CMDCID
     C                   MOVEL     *BLANKS       CMFOLD
     C                   MOVEL     *BLANKS       CMFL05
     C                   MOVEL     'SCCV3R'      CMPGM
     C                   MOVE      CMSEQ#        XXSEQ#            5
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     *BLANKS       CMUNCN
     C                   MOVEL     *BLANKS       CMIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C                   END
     C                   UPDATE    PCM01
     C                   ENDIF
     C*** CHECK FOR OPTICAL (DOC LOC UPDATE)
     C     CVFL05        IFEQ      'O'
     C                   EXSR      FIXDL
     C                   ENDIF
     C*
     C*** 4) MARK CVPRCS AS A "U" FOR UPDATED DATABASE
      *
     C*
     C                   MOVEL     'U'           CVPRCS
     C*
     C                   UPDATE    PCV01
      *
     C     NXTONE        TAG
     C                   READ      PCV01                                  60
     C                   ENDDO
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C********
     C*** GET/UPDATE THE DL RECORD IF IT WAS AN OPTICAL
     C     FIXDL         BEGSR
     C     DLKY          CHAIN     PDL01                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     'I'           DLSTAT
     C                   UPDATE    PDL01
     C                   ENDIF
     C*
     C                   ENDSR
