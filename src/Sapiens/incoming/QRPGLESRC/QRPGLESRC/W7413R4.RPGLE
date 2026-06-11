     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7413R4 - POPULATE FOLDER FIELD IN WXFMP FOR DTM
     H*****************************************************************
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7413C2                                                 **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:65                                          **
     H**   CREATED:   01/08/04      PROJECT: SY059                   **
     H**                                                             **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/15/04 C:33 Project: SY059                                 *
     H*                Allow for testing on Fiserv development box.   *
     H*                                                               *
     H*  07/09/04 C:09 Project: SY100                                 *
     H*                Add additional logic to handle client folders  *
     H*                                                               *
     H*  01/20/04 C:65  PROJECT: SY059                                *
     H*                 POPULATE DOCUMENT ID FIELD SINCE THE          *
     H*                 ENDORSEMENT FORM DOES NOT ALWAYS MATCH THE    *
     H*                 NAME OF THE DOCUMENT.                         *
     H*                                                               *
     H*****************************************************************
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H*****************************************************************
     FWXFMP     UF A E             DISK
     FWTENL     IF   E           K DISK

     D*** BEGIN ADD ***  01/20/04  ************************************
     D                 DS
     D  FMFORM                 1     15
     D  XXFRM1                 1      8
     D  XXFRM2                 9     11

     I*** END ADD *****  01/20/04  ************************************

     C*
     C*  KEY LIST FOR ENDORSEMENT FORM TABLE FILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    FMCO#
     C                   KFLD                    FMFND
     C                   KFLD                    FMFORM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQMODE            1
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           40
     C*
     C     QQMODE        IFNE      'E'
     C*
     C                   CLEAR                   PFM01
     C*** BEGIN CHG ***  01/20/04  ************************************
     C*********            MOVELQQDCID    FMFORM
     C                   MOVEL(P)  QQDCID        FMDCID
     C*** END CHG *****  01/20/04  ************************************
     C*** BEGIN CHG *** 07/09/04 **************************************
     C*********            MOVELQQFOLD    FMFOLD
     C*
     C***  Begin Add  ***  12/15/04  **************************************
     C                   MOVEL(P)  QQFOLD        ##FOLD           12
     C*
     C                   CALL      'DTCHKFLR'
     C                   PARM                    ##FOLD
     C*
     C                   MOVEL(P)  ##FOLD        QQFOLD
     C***  End   Add  ***  12/15/04  **************************************
     C*
     C                   CALL      'SA039R'
     C                   PARM      QQFOLD        QQFLDR           80
     C                   MOVEL     QQFLDR        FMFOLD
     C*** END   CHG *** 07/09/04 **************************************
     C                   WRITE     PFM01
     C*
     C                   ELSE
     C*
     C                   READ      PFM01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ENKY          CHAIN     PEN01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C***  Begin Add  ***  12/15/04  **************************************
     C                   MOVEL(P)  ENFLDR        ##FOLD
     C*
     C                   CALL      'DTCHKFLR'
     C                   PARM                    ##FOLD
     C*
     C                   MOVEL(P)  ##FOLD        ENFLDR
     C***  End   Add  ***  12/15/04  **************************************
     C*
     C*** BEGIN ADD *** 07/09/04 **************************************
     C                   CALL      'SA039R'
     C                   PARM      ENFLDR        QQFLDR           80
     C                   MOVEL     QQFLDR        ENFLDR
     C*** END   ADD *** 07/09/04 **************************************
     C                   MOVEL     ENFLDR        FMFOLD
     C*
     C*** BEGIN ADD ***  01/20/04  ************************************
     C     XXFRM2        IFNE      *BLANKS
     C                   MOVEL(P)  XXFRM1        FMDCID
     C                   CAT       '.':0         FMDCID
     C                   CAT       XXFRM2:0      FMDCID
     C                   ELSE
     C                   MOVEL(P)  FMFORM        FMDCID
     C                   ENDIF
     C*** END ADD *****  01/20/04  ************************************
     C*
     C                   UPDATE    PFM01
     C                   ENDIF
     C*
     C                   READ      PFM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  QQMODE IFNE
     C*
     C                   SETON                                        LR
