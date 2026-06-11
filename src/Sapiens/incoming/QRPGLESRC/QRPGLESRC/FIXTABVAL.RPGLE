     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title FIXTABVAL - LOAD WTGTP AND WTGIP FILES
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      * CALLED FROM:                                                  *
      * CALLED TO:                                                    *
      *                                                               *
      * Created 08/31/10  C:255  Project: SY190                       *
      *****************************************************************
      * Revision:                                                     *
      * 09/17/10   C:180    PRISS C133WKA                             *
      *                                                               *
      *                                                               *
      * 09/16/10 C:255 PROJECT: SY190(PRISS#125)                      *
      *                ADDED CHANGE TO READ COMPANY NO. FROM SMCOP FI-*
      *                LE INSTEAD HARD CODE 777.                      *
      *                                                               *
      *****************************************************************
     FWTGTL     UF A E           K DISK
     FWTGIL     UF A E           K DISK
     F*** BEGIN ADD *** 09/16/10 **************************************
     FSMCOL     IF   E           K DISK
     F*** END   ADD *** 09/16/10 **************************************
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D XXDT            s              7  0
     DPSDS           ESDS                  Extname(SZQ1P)
     D@@CO#            S                   LIKE(GICO#)
     D##ID             S                   LIKE(GITBID)
     D##KEY            S                   LIKE(GTKEY)
     D*
    C**********************************************************************
    C*Parm Lists/Key Lists
    C**********************************************************************
     C     TBKEY1        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    ##ID
     C*
     C     TBKEY2        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    ##ID
     C                   KFLD                    ##KEY
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C*** BEG   CHG *** 09/16/10 **************************************
     C*******            MOVE      777           @@CO#
     C     *LOVAL        SETLL     SMCOL
     C                   READ      PCO01                                  60
     C                   DOW       *IN60 = *OFF
     C                   MOVE      COCO#         @@CO#
     C*** END   CHG *** 09/16/10 **************************************
     C* FOR SCLS & 200
     C                   EVAL      ##ID='SCLS'
     C                   EVAL      ##KEY='200'
     C                   EXSR      WRT
     C*FOR SCLS & 220
     C                   EVAL      ##ID='SCLS'
     C                   EVAL      ##KEY='220'
     C                   EXSR      WRT
     C*FOR SCLS & 225
     C                   EVAL      ##ID='SCLS'
     C                   EVAL      ##KEY='225'
     C                   EXSR      WRT
     C*FOR BTYP & AG
     C                   EVAL      ##ID='BTYP'
     C                   EVAL      ##KEY='AG'
     C                   EXSR      WRT
     C*FOR BTYP & IC
     C                   EVAL      ##ID='BTYP'
     C                   EVAL      ##KEY='IC'
     C                   EXSR      WRT
     C*FOR BTYP & RP
     C                   EVAL      ##ID='BTYP'
     C                   EVAL      ##KEY='RP'
     C                   EXSR      WRT
     C*FOR BTYP & WC
     C                   EVAL      ##ID='BTYP'
     C                   EVAL      ##KEY='WC'
     C                   EXSR      WRT
     C*FOR BTYP & WP
     C                   EVAL      ##ID='BTYP'
     C                   EVAL      ##KEY='WP'
     C                   EXSR      WRT
     C*For BCLV & D
     C                   EVAL      ##ID='BCLV'
     C                   EVAL      ##KEY='D'
     C                   EXSR      WRT
     C*For BCLV & E
     C                   EVAL      ##ID='BCLV'
     C                   EVAL      ##KEY='E'
     C                   EXSR      WRT
     C*For BCLV & M
     C                   EVAL      ##ID='BCLV'
     C                   EVAL      ##KEY='M'
     C                   EXSR      WRT
     C*For BCLV & S
     C                   EVAL      ##ID='BCLV'
     C                   EVAL      ##KEY='S'
     C                   EXSR      WRT
     C*For BCLV & J
     C                   EVAL      ##ID='BCLV'
     C                   EVAL      ##KEY='J'
     C                   EXSR      WRT
     C*
      ***** BEGIN ADD ***** 09/17/10 ***************************************
     C*FOR EIDQ & A
     C                   EVAL      ##ID='EIDQ'
     C                   EVAL      ##KEY='A'
     C                   EXSR      WRT
     C*FOR EIDQ & E
     C                   EVAL      ##ID='EIDQ'
     C                   EVAL      ##KEY='E'
     C                   EXSR      WRT
     C*FOR EIDQ & G
     C                   EVAL      ##ID='EIDQ'
     C                   EVAL      ##KEY='G'
     C                   EXSR      WRT
     C*FOR EIDQ & P
     C                   EVAL      ##ID='EIDQ'
     C                   EVAL      ##KEY='P'
     C                   EXSR      WRT
     C*FOR EIDQ & S
     C                   EVAL      ##ID='EIDQ'
     C                   EVAL      ##KEY='S'
     C                   EXSR      WRT
      ***** END   ADD ***** 09/17/10 ***************************************
     C*** BEG   ADD *** 09/16/10 **************************************
     C                   READ      PCO01                                  60
     C                   ENDDO
     C*** END   ADD *** 09/16/10 **************************************
     C*
     C                   EVAL      *INLR = *ON
     C*=====================================================================
     C     WRT           BEGSR
     C*=====================================================================
     C                   EXSR      INITZ
     C     TBKEY1        CHAIN     PGI01                              70
     C                   IF        *IN70= *ON
     C                   CLEAR                   PGI01
     C*
     C                   EVAL      GICO#=@@CO#
     C                   EVAL      GITBID=##ID
     C*
     C                   select
     C                   when      ##ID ='SCLS'
     C                   EVAL      GIDESC='SERVICE CLASS (EFT)'
     C                   EVAL      GIFN01='SCLS'
     C                   when      ##ID ='BTYP'
     C                   EVAL      GIDESC= 'BANK TYPE CODE'
     C                   EVAL      GIFN01='BTYP'
     C                   when      ##ID ='BCLV'
     C                   EVAL      GIDESC= 'BILLING CALCULATION LEVEL'
     C                   EVAL      GIFN01='BCLV'
      ***** BEGIN ADD ***** 09/17/10 ***************************************
     C                   when      ##ID ='EIDQ'
     C                   EVAL      GIDESC= 'EMPLOYEE ID TYPE'
     C                   EVAL      GIFN01='EIDQ'
      ***** END   ADD ***** 09/17/10 ***************************************
     C                   endsl
     C*
     C                   EVAL      GISTAT='A'
     C                   EVAL      GIUSER=Q1USER
     C                   EVAL      GIPGM =Q1PGM
     C                   MOVE      dstime        GITIME
     C                   MOVE      xxdt          GIDT
     C*
     C                   WRITE     PGI01
     C                   ENDIF
     C*
     C     TBKEY2        CHAIN     PGT01                              71
     C                   IF        *IN71= *ON
     C                   CLEAR                   PGT01
     C*
     C                   EVAL      GTCO#=@@CO#
     C                   EVAL      GTTBID=##ID
     C                   EVAL      GTKEY =##KEY
     C*
     C                   select
     C*
     C                   when      ##ID ='SCLS'
     C                   select
     C                   when      ##KEY='200'
     C                   EVAL      GTDESC = 'Mixed debits and credits'
     C                   when      ##KEY='220'
     C                   EVAL      GTDESC = 'Credits Only'
     C                   when      ##KEY='225'
     C                   EVAL      GTDESC = 'Debits Only'
     C                   endsl
     C*
     C                   when      ##ID ='BTYP'
     C                   select
     C                   when      ##KEY='AG'
     C                   EVAL      GTDESC = 'Agent Commissions'
     C                   when      ##KEY='IC'
     C                   EVAL      GTDESC = 'Independent Cash'
     C                   when      ##KEY='RP'
     C                   EVAL      GTDESC = 'Return Funds'
     C                   when      ##KEY='WC'
     C                   EVAL      GTDESC = 'Worker'+''''+'s Comp (Claims)'
     C                   when      ##KEY='WP'
     C                   EVAL      GTDESC = 'Worker'+''''+'s Comp (Premium)'
     C                   endsl
     C*
     C                   when      ##ID ='BCLV'
     C                   select
     C                   when      ##KEY='D'
     C                   eval      GTDESC = 'Discounted Premium'
     C                   when      ##KEY='E'
     C                   eval      GTDESC = 'Exp. Modified Premium'
     C                   when      ##KEY='M'
     C                   eval      GTDESC = 'Manual Premium'
     C                   when      ##KEY='S'
     C                   eval      GTDESC = 'Standard Premium'
     C                   when      ##KEY='J'
     C                   eval      GTDESC = 'Subject Premium'
     C                   endsl
      ***** BEGIN ADD ***** 09/17/10 ***************************************
     C                   when      ##ID ='EIDQ'
     C                   select
     C                   when      ##KEY='A'
     C                   eval      GTDESC = 'Employee ID Assigned ' +
     C                                      'by Jurisdiction'
     C                   when      ##KEY='E'
     C                   eval      GTDESC = 'Employee Employment Visa'
     C                   when      ##KEY='G'
     C                   eval      GTDESC = 'Employee Green Card'
     C                   when      ##KEY='P'
     C                   eval      GTDESC = 'Employee Passport Number'
     C                   when      ##KEY='S'
     C                   eval      GTDESC = 'Employee Social Security Number'
     C                   endsl
      ***** END   ADD ***** 09/17/10 ***************************************
     C*
     C                   endsl
     C*
     C                   EVAL      GTSTAT='A'
     C                   EVAL      GTUSER =Q1USER
     C                   EVAL      GTPGM  =Q1PGM
     C                   MOVE      dstime        GTTIME
     C                   MOVE      xxdt          GTDT
     C*
     C                   WRITE     PGT01
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*=====================================================================
     C     initz         begsr
     C*=====================================================================
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         XXDT
     C                   endsr
