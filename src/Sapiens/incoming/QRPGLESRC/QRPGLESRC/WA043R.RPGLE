     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA043R - API to Retrieve Work Information
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*    This program will retrieve work information using WMCMP    *
     H*    CO#/FND/FYR/CASE key information.                          *
     H*    If found, return work information.                         *
     H*    If NOT found, 'F' will be returned.                        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F0263R - Work Information                                  *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/09/00  PROGRAMMER: 62  PROJECT: MCMNT            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/04/09 C:062  Project: WC1807                               *
     H*                 IGNORE E TYPE WORK HISTORY ENTRIES            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FWMCML1    IF   E           K DISK
     FWMWHL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     WHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     WHKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDATE
     C*
     C     *LIKE         DEFINE    WHCO#         KKCO#
     C     *LIKE         DEFINE    WHFND         KKFND
     C     *LIKE         DEFINE    WHFYR         KKFYR
     C     *LIKE         DEFINE    WHCASE        KKCASE
     C     *LIKE         DEFINE    WHDATE        KKDATE
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@OCDS           40            Occupation
     C                   PARM                    @@HIDT            7 0          Hire Date
     C                   PARM                    @@ESTS            2            Emplyr Sts
     C                   PARM                    @@EDSC           40            Emplyr Sts Dsc
     C                   PARM                    @@DAYS            1 0          Days/Week
     C                   PARM                    @@HRSD            3 0          Hours/Day
     C                   PARM                    @@SHFT            1            Shift
     C                   PARM                    @@SDSC           40            Shift Desc
     C                   PARM                    @@DTLW            7 0          Dt Last Worked
     C                   PARM                    @@WKST            1            Work Status
     C                   PARM                    @@DATE            7 0          Status Date
     C                   PARM                    @@RTCD            1
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     *HIVAL        KKDATE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     CMOCDS        @@OCDS                         Occupation
     C                   Z-ADD     CMHIDT        @@HIDT                         Hire Date
     C                   MOVEL     CMESTS        @@ESTS                         Emplyr Sts
     C*
     C                   MOVEL(P)  'EMST'        KKTBID
     C                   MOVEL(P)  CMESTS        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        @@EDSC
     C   99              MOVEL(P)  *BLANKS       @@EDSC
     C*
     C                   Z-ADD     CMDAYS        @@DAYS                         Days/Week
     C                   Z-ADD     CMHRSD        @@HRSD                         Hours/Day
     C                   MOVEL     CMSHFT        @@SHFT                         Shift
     C*
     C                   MOVEL(P)  'SHFT'        KKTBID
     C                   MOVEL(P)  CMSHFT        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        @@SDSC
     C   99              MOVEL(P)  *BLANKS       @@SDSC
     C*
     C                   Z-ADD     CMDTLW        @@DTLW                         Dt Last Worked
     C*
     C     WHKY2         SETGT     PWH01
     C     WHKY          READPE    PWH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     WHWKST        IFEQ      'L'
     C*
     C                   Z-ADD     WHDATE        @@DTLW
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     WHKY          READPE    PWH01                                  99
     C*
     C                   ENDDO
     C*
     C     WHKY2         SETGT     PWH01
     C     WHKY          READPE    PWH01                                  99
     C*
     C*** BEGIN CHG *** 05/04/09 **************************************
     C********** *IN99     IFEQ *OFF
     C     *IN99         DOWEQ     *OFF
     C*** END   CHG *** 05/04/09 **************************************
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*
     C* EXCLUDE E TYPES
     C*
     C     WHWKST        IFEQ      'R'
     C     WHWKST        OREQ      'L'
     C*** END   ADD *** 05/04/09 **************************************
     C                   MOVEL     WHWKST        @@WKST
     C                   Z-ADD     WHDATE        @@DATE
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   LEAVE
     C                   ENDIF
     C*
     C     WHKY          READPE    PWH01                                  99
     C                   ENDDO
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*** BEGIN DEL *** 05/04/09 **************************************
     C**********           ENDIF
     C*** END   DEL *** 05/04/09 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF
     C*
     C                   RETURN
