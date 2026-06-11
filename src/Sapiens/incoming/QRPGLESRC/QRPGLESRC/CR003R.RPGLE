     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*
     H*  PROGRAM SUBROUTINE CR003R - BUILD AGENT NAME PROGRAM
     H*****************************************************************
     H*  THIS SUBPROGRAM WILL TAKE THE INCOMING PARMS OF PROGRAM AND
     H*  DARTS KEY; RETRIEVE AGENT NAME FROM WORK COMP FILE AND
     H*  RETURN AGENT NAME TO SRKCVT PGM.
     H*****************************************************************
     H*  PROGRAMMER: 09                                               *
     H*  CREATED:    09/23/97             PROJECT: WC577              *
     H*****************************************************************
     H*  REVISIONS:                                                   *
      * **                                                                    **
      * ** C:002                                                              **
      * ** DR214                                                              **
      * ** 10/24/06                                                           **
      *    Changes for idarts compatibility                                   **
      *** RETURN ADDITIONAL INFORMATION TO THE CALLER (2 79 CHR FLDS)     ******
     F*
     H*                                                               *
     H*                                                               *
     H*   6/09/09  C:220  SUPPORT: 68708                              *
     H*                   CORRECT PROGRAM TO RUN PROPERLY             *
     H*                   DURING THE iDARRT CALL.                     *
     H*                                                               *
     H*   2/19/06  C:002  VDARTS                                      *
     H*                   CHANGE PROGRAM TO USE RETRN INSTEAD OF *INLR*
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*  11/21/01  C:128  Support: OP109/29973                        *
     H*                   Inz all data structures that contain        *
     H*                   numeric data to prevent data decimal error. *
     H*                                                               *
     H*  03/20/00  C:15   Support: DR048/22271                        *
     H*                   Added code to prevent data-decimal error.   *
     H*****************************************************************
     F* WORK COMP AGENT MASTER FILE       (PAG01)
     FWMAGL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D*
     D* DATA STRUCTURE FOR KEY FOR WORK COMP
     D                 DS                  INZ
     D  XXKEY                  1     79
     D  XXFEIN                 1      9  0
     D  XXMOD                 10     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*        * * *  CALCULATION SPECIFICATIONS  * * *
     C*****************************************************************
     C*
     C* KEY LIST FOR THE WORK COMP EMPLOYER MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPGM            10            PROGRAM NUMBER
     C                   PARM                    QQKEY            79            DARTS' KEY
     C                   PARM                    QQNAME           79            NAME
     I*** BEGIN ADD *** 10/24/06 **************************************
     C                   PARM                    Q1DTA1           79            DTA 1
     C                   PARM                    Q1DTA2           79            DTA 2
     I*** END   ADD *** 10/24/06 **************************************
     C*
     C*** BEGIN ADD *** 03/20/00 **************************************
     C     QQKEY         IFNE      *BLANKS
     C*** END ADD ***** 03/20/00 **************************************
     C*
     C                   MOVEL     QQKEY         XXKEY
     C*
     C* SET-UP AND INITIALIZE
     C*
     C                   MOVE      *BLANKS       QQNAME
     C     UPKY          CHAIN     PUP01                              60
     C*** BEGIN ADD *** 06/09/09 **************************************
     C     *IN60         IFEQ      '0'
     C*** END   ADD *** 06/09/09 **************************************
     C     AGKY          CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AGNAME        QQNAME
     C                   END
     C*** BEGIN ADD *** 06/09/09 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/09/09 **************************************
     C*
     C*** BEGIN ADD *** 03/20/00 **************************************
     C                   ENDIF
     C*** END ADD ***** 03/20/00 **************************************
     C*** BEGIN ADD *** 10/24/06 **************************************
     C     Q1PRMS        IFGT      3
     **
     C***  THE ADDITIONAL FIELDS FOR DISPLAY WERE REQUESTED (DTA1/DTA2)
     C***    THESE CAN BE FILLED WITH TEXT AND DATA FROM AGT  FILES   INCLUDING
      ***    INFO NOT CURRENTLY DISPLAYED (IF APPLICABLE) SEE CR001R FOR EXAMPLE
      ***    SIMPLY CONCAT THE APPROPRIATE TEXT AND DATA INTO DTA1 AND/OR
      ***    DTA2 AND IT WILL BE DISPLAYED AS PART OF THE HEADING FOR IDARTS
      ***
      *
     C                   ENDIF
     C*** END   ADD *** 10/24/06 **************************************
     C*
     C* END OF PROGRAM
     C****** ADD 02/19/06 *********************************************
     C*
     C                   RETURN
     C***END ADD 02/19/06 *********************************************
     C*
     C                   MOVE      '1'           *INLR
