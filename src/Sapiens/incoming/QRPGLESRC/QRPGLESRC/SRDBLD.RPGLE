     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRDBLD - BUILD DOCUMENT SUBPROGRAM
     H*
     H*****************************************************************
     H* NOTICE:::::::
     H*  PROGRAM MUST BE PASSED THE COMPLETE FOLDER NAME. (SRFPTH)
     H*  PROGRAM MUST BE PASSED THE REMARK TYPE.          (SRSUB)
     H*   IF FOLDER NAME IS BLANK, SRTERM IS RETURNED = '5'
     H*   IF REMARK TYPE IS BLANK, SRTERM IS RETURNED = '6'
     H*****************************************************************
     H*
     H*****************************************************************
     H*  THIS SUBPROGRAM WILL TAKE THE INCOMING PARMS OF FOLDER AND
     H*  REMARK TYPE AND BUILD A DOCUMENT NAME.
     H*
     H*****************************************************************
     H*  PROGRAMMER:  LORI J. QUINN
     H*  DATE:        JUNE 27, 1991
     H*  CLIENT:      003
     H*****************************************************************
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 07/37/98 C:02 - PROJ:
     H*                 1. ADDED ENTRY PARM QQMS(MS OFFICE) IF THIS
     H*                    FLAG IS A 'Y', ADD .DOC AS THE EXTENSION TO
     H*                    THE DOCUMENT NAME.
     H*
     H* 12/03/92 C:10 - Proj:
     H*                 1.  Added code to keep DARTS program from
     H*                     bombing during a F20=Autobuild function.
     H*                     If the return code from program
     H*                     SDSPFLD1 is a 4 (i.e., global monitor msg),
     H*                     program is terminated.
     H*****************************************************************
     H*
     F*
     F* OUTFILE OF DOCUMENTS IN DESCENDING ORDER
     FSAFLL1    IF   E           K DISK    USROPN
     F**** ADD MS OFFICE INTEGRATION *********** 7/98 ********
     FSSYLF     IF   E           K DISK

     D*
     D* ARRAY FOR DOCUMENTS AFTER DROPPING 2-BYTE ALPHA PREFIX
     D DOC             S              1    DIM(10)                                DOCUMENT
     D NUM             S              1    DIM(10) CTDATA PERRCD(1)
     D QCM             S             36    DIM(2) CTDATA PERRCD(1)              CMD FOR QCMDEXC
     D*
     D*
     D* DATA STRUCTURE FOR DOCUMENT NAME
     D                 DS
     D  DOCNAM                 1     12
     D  XZPREF                 1      2
     D  XZDOC                  3      7
     D  XXDOC                  3      7
     D*** QCAEXEC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     36
     D  @@MBR                 25     34
     D*** QCAEXEC CMD STRING MAPPED
     D                 DS
     D  @1CMD                  1     34
     D  @1MBR                 23     32

     I*

     C*** BEGIN ADD *** 07/26/98 **************************************
     C*   KEY LIST FOR THE SYSTEM CTL FILE
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** END ADD *** 07/26/98 ****************************************
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRFPTH           40            FULL FOLDER NAM
     C                   PARM                    SRSUB             2            SUB-ARA(COM CD)
     C                   PARM                    SRDOC            12            DOCUMENT
     C                   PARM                    SRTERM            1            SET LR FLAG.
     C*
     C* SET-UP AND INITIALIZE
     C*
     C                   MOVE      *BLANKS       DOC
     C                   MOVE      *BLANKS       SRDOC
     C                   Z-ADD     0             XXDOCN            5 0
     C                   MOVE      *BLANKS       QQDOC             7
     C                   MOVE      *BLANKS       QQFOLD           80
     C                   MOVEL     SRFPTH        QQFOLD
     C*
     C*** ADD MSOFFICE INTEGRATION DR063 ****** 8/98 ******
     C                   MOVEL     'SC'          XXSYK
     C*
     C* LOOK AT DARTS SYSTEM CONTROL FILE TO DETERMINE WHAT'S INSTALLED
     C*
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRFL06        QQMS              1
     C                   ELSE
     C                   MOVEL     *BLANKS       QQMS
     C                   ENDIF
     C*** END ADD MSOFFICE INTEGRATION DR063 ****** 8/98 ******
     C*
     C* VALIDATE THAT FOLDER AND REMARK TYPE <> *BLANKS
     C*
     C     SRFPTH        IFEQ      *BLANKS
     C                   MOVE      '5'           SRTERM
     C                   GOTO      RETURN
     C                   END
     C*
     C     SRSUB         IFEQ      *BLANKS
     C                   MOVE      '6'           SRTERM
     C                   GOTO      RETURN
     C                   END
     C                   MOVE      ' '           SRTERM
     C*
     C* CALL CL PGM TO BUILD OUTFILE WITH DOCUMENTS IN THE FOLDER
     C*
     C                   CALL      'SDSPFLR1'
     C                   PARM                    QQFOLD
     C                   PARM                    SRTERM
     C                   PARM                    QQMBR            10
     C     SRTERM        IFEQ      '4'
     C                   GOTO      ERROR1
     C                   END
     C     SRTERM        IFNE      ' '
     C                   GOTO      RETURN
     C                   END
     C*
     C* OVERRIDE FILE TO CORRECT MEMBER NAME
     C*
     C                   MOVE      *BLANKS       @@CMD
     C                   Z-ADD     36            LENGTH
     C                   MOVEL     QCM(1)        @@CMD
     C                   MOVEL     QQMBR         @@MBR
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   OPEN      SAFLL1
     C*
     C* READ FILE TO FIND FIRST RECORD THAT HAS THE SAME PREFIX AS THE
     C* REMARK TYPE
     C*
     C                   READ      DOCDTL                                 50
     C     *IN50         DOWEQ     '0'
     C     SRSUB         IFEQ      XZPREF
     C*
     C* DROP LEADING PREFIX AND SEARCH NUM ARRARY TO CHECK IF
     C* DOCUMENT ID IS NUMERIC.  IF DOC ID IS NUMERIC, THEN ADD 1
     C* TO GET NEXT DOC ID.  IF DOC ID IS NOT NUMERIC, THEN CREATE
     C* DIC ID 1.
     C*
     C                   MOVEA     XZDOC         DOC
     C                   Z-ADD     1             X                 3 0
     C                   MOVE      '1'           *IN55
     C     X             DOWLT     11
     C     *IN55         ANDEQ     '1'
     C     DOC(X)        ANDNE     ' '
     C     DOC(X)        LOOKUP    NUM                                    55
     C                   ADD       1             X
     C                   END                                                    DO END
     C*
     C* IF *IN55='1' THEN THIS A VALID DOC EXISTS THAT IS NUMERIC
     C*  SO ADD 1 TO DOC ID TO GET NEW DOC ID.
     C*
     C     *IN55         IFEQ      '1'
     C                   MOVE      XXDOC         XXDOCN
     C                   ADD       1             XXDOCN
     C                   MOVEL     SRSUB         QQDOC
     C                   MOVE      XXDOCN        QQDOC
     C                   MOVEL     QQDOC         SRDOC
     C*** BEGIN ADD *** 07/27/98 **************************************
     C     QQMS          IFEQ      'Y'
     C     SRDOC         CAT       '.DOC':0      SRDOC
     C                   END
     C*** END ADD *** 07/27/98 ****************************************
     C                   GOTO      RETURN
     C*
     C                   ELSE
     C*
     C* ELSE
     C*  NO VALID NUMERIC DOC ID EXISTS, SO START DOC ID AT DOC 1
     C*
     C                   Z-ADD     1             XXDOCN
     C                   MOVEL     SRSUB         QQDOC
     C                   MOVE      XXDOCN        QQDOC
     C                   MOVEL     QQDOC         SRDOC
     C*** BEGIN ADD *** 07/27/98 **************************************
     C     QQMS          IFEQ      'Y'
     C     SRDOC         CAT       '.DOC':0      SRDOC
     C                   END
     C*** END ADD *** 07/27/98 ****************************************
     C                   GOTO      RETURN
     C                   END                                                    VALID DOC ID
     C*
     C                   END                                                    PREFIX = SUB
     C*
     C* READ NEXT RECORD IN DOCUMENT FILE
     C*
     C                   READ      DOCDTL                                 50
     C                   END                                                    END READ 50
     C*
     C* READ ENTIRE FILE AND DID NOT FIND A RECORD WITH MATCHING
     C* PREFIX - SO NOW CREATE DOC ID 1
     C*
     C                   Z-ADD     1             XXDOCN
     C                   MOVEL     SRSUB         QQDOC
     C                   MOVE      XXDOCN        QQDOC
     C                   MOVEL     QQDOC         SRDOC
     C*** BEGIN ADD *** 07/27/98 **************************************
     C     QQMS          IFEQ      'Y'
     C     SRDOC         CAT       '.DOC':0      SRDOC
     C                   END
     C*** END ADD *** 07/27/98 ****************************************
     C*
     C     RETURN        TAG
     C*
     C* REMOVE OUTFILE MEMBER
     C*
     C                   CLOSE     SAFLL1
     C                   MOVE      *BLANKS       @1CMD
     C                   Z-ADD     34            LENGTH
     C                   MOVEL     QCM(2)        @1CMD
     C                   MOVEL     QQMBR         @1MBR
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    @1CMD
     C                   PARM                    LENGTH           15 5
     C*
     C     ERROR1        TAG
     C*
     C     SRTERM        IFNE      ' '
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
     C/EJECT
**
0
1
2
3
4
5
6
7
8
9
** QCM - CMD FOR QCMDEXC
OVRDBF FILE(SAFLL1) MBR(          )
RMVM FILE(SAFLL1) MBR(          )
