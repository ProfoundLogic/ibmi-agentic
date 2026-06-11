     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRPBFL - BUILD FOLDER SUBPROGRAM
     H*
     H*****************************************************************
     H* NOTICE:
     H*   IF FEIN IS 'P', SRTERM IS RETURNED = '1'
     H*   IF MOD IS 'M', SRTERM IS RETURNED = '2'
     H*   IF FOLDER COULD NOT BE CREATED, SRTERM IS RETURNED = '3'
     H*****************************************************************
     H*
     H*****************************************************************
     H*
     H*  THIS SUBPROGRAM WILL TAKE PARMS FEIN AND MOD; AND
     H*    CREATE FOLDER "FEIN/MOD" IF IT DOES NOT ALREADY EXIST
     H*
     H*  Folder Path System: FEIN/MOD/DOCUMENT
     H*
     H*     FEIN = Pnnnnnn.nnn
     H*               'P' for provider,  nnnnnn.nnn = FEIN
     H*
     H*     MOD  = Mxxx
     H*               'M' for modifier,  xxx = Mod
     H*
     H*     DOCUMENT = SSxxxxx       (refer calling pgm SCEXITR)
     H*               'SS' same as sub-area which corresponds to the
     H*                    remark type.
     H*               xxxxx is the next sequence nubmer unique to the
     H*                    FEIN/MOD documents.
     H*
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY: SCEXITR - Auto Build Folder & Document            *
     H*                                                               *
     H*  CALLS TO:  SRVALFLR                                          *
     H*             SRADDPFL - Create Folder for Provider             *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  06/03/97 C:61 PROJECT 48WGQ                                  *
     H*                Pulled code from WSRC2                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  XX/XX/XX C:XX PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H*  06/08/04 C:62 Project: SY100                                 *
     H*                Add additional logic to handle client folders  *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*
      *
      * ENTRY PARAMETERS
      *

     C     *ENTRY        PLIST
     C                   PARM                    SRFEIN           12
     C                   PARM                    SRMOD             4
     C                   PARM                    SRFPTH           40            FULL FOLDER NAM
     C                   PARM                    SRTERM            1            SET LR FLAG.
     C*** BEGIN ADD *** 06/08/04 **************************************
     C*
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUF             3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 06/08/04 **************************************
      *
      * SET-UP AND INITIALIZE
      *
     C                   MOVE      *BLANKS       SRFPTH
     C                   MOVE      *BLANKS       SRTERM
      *
      * CREATE FOLDER PATH NAME
      *
     C*** BEGIN ADD *** 06/08/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         XXPATH
     C                   CAT(P)    SRFEIN:0      XXPATH
     C                   MOVEL(P)  SRFEIN        HLPATH           40
     C                   ELSE
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  SRFEIN        XXPATH           40
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  SRFEIN        HLPATH
     C                   ENDIF
     C*** END   ADD *** 06/08/04 **************************************
     C                   CAT       '/':0         XXPATH
     C                   CAT       SRMOD:0       XXPATH
      *
      * ATTEMPT TO CREATE FOLDER
      *
     C*** BEGIN ADD *** 06/08/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C                   MOVEL(P)  ##PFLR        Q1INFL
     C                   ELSE
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  '*NONE'       Q1INFL
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  SRFEIN        Q1PTH            12
     C                   CALL      'SADDFLR'
     C                   PARM                    Q1INFL           63
     C                   PARM                    Q1PTH
     C                   PARM      *BLANKS       Q1RTC             1
      *
     C     Q1RTC         IFEQ      *BLANKS
     C     Q1RTC         OREQ      '1'
     C*** BEGIN ADD *** 06/08/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         Q1INFL
     C                   CAT       SRFEIN:0      Q1INFL
     C                   ELSE
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  SRFEIN        Q1INFL
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  SRMOD         Q1PTH            12
     C                   CALL      'SADDFLR'
     C                   PARM                    Q1INFL           63
     C                   PARM                    Q1PTH
     C                   PARM      *BLANKS       Q1RTC             1
     C                   ENDIF
     C*
     C* IF RETURN CODE = 1 THEN FOLDER ALREADY EXISTED
     C* IF RETURN CODE IS ANYTHING OTHER THAN 1 OR *BLANKS THEN WE
     C*  COULD NOT CREATE THE FOLDER- RETURN TO CALLING PGM WITH
     C*  SRTERM = 3
     C*
     C     Q1RTC         IFNE      *BLANKS
     C     Q1RTC         IFNE      '1'
     C                   MOVE      '3'           SRTERM
     C                   END
     C                   END
     C*
     C     RETURN        TAG
     C*
     C* MOVE FULL FOLDER NAME TO OUT-GOING PARM
     C*
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   MOVEL     HLPATH        XXPATH
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  XXPATH        SRFPTH
     C*
     C                   MOVE      '1'           *INLR
