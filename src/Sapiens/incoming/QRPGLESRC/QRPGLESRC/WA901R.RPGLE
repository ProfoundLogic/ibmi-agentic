     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H* *****************************************************************
     H/TITLE WA901R - Apportionment Action Program
     H*                                                                 *
     H*******************************************************************
     H* Description: This program will try to calculate the           * *
     H*              apportionment amount.  If it can't then it       * *
     H*              returns to the calling program.  If it can       * *
     H*              then it displays a window that prompts the       * *
     H*              user to decide if they want to apply the         *-*-
     H*              apportionment value.                             * *
     H*                                                               * *
     H*  Called From:  W0080R - System Generated Check Process        * *
     H*                W0120R - Manual Payment Request                * *
     H*                W8080R - Reoccuring Comp Payment Request       * *
     H*                                                               * *
     H*  Calls To   :  None                                           * *
     H*                                                               * *
     H*******************************************************************
     H* Created on 03/20/2003 by c:62 for Project 129MBH              * *
     H*******************************************************************
     H* Revisions:                                                    * *
     H*                                                               * *
     H* 11/25/13 C:282 Project : 129WSC                               *
     H*                Code moved form Client 129 to Base.            *
     H*                                                               * *
     H* 09/23/03 C:53 SUPPORT: WC129 # 38762                          *
     H*               Rename record format                            *
     H*                                                               * *
     H*******************************************************************
     FWA901F    CF   E             WORKSTN
      *** Beg Add ***  11/25/13  ******************************************************
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      *** End Add ***  11/25/13  ***********************************************HEADING
      /ENDIF
     F                                     INFDS(FIDS01)
      *** Beg CHG ***  11/25/13  ******************************************************
     F*****WMCMPX    IF   E           K DISK
     FWMCML1    IF   E           K DISK
      *** End CHG ***  11/25/13  ******************************************************
     FWTABL     IF   E           K DISK
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     C*
     C     *ENTRY        PLIST
      *** Beg Del ***  11/25/13  ******************************************************
     C*****              PARM      *INKL         @@KL              1
      *** End Del ***  11/25/13  ******************************************************
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@AMT             9 2
     C                   PARM                    @@BR2             2
     C                   PARM                    @@FRDT            8 0
     C                   PARM                    @@AAMT            9 2
     C                   PARM                    @@RTCD            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMAEFF        XXFRDT
     C     *LIKE         DEFINE    B2KEY         KKKEY
     C*
     C                   EXSR      CHKAPP
     C     *IN90         IFEQ      *ON
      *** Beg Del ***  11/25/13  ******************************************************
     C******IN91         DOUEQ     *OFF
     C******INKL         OREQ      *ON
      *** End Del ***  11/25/13  ******************************************************
     C     btnAccept     DOUEQ     *ON
     C     btnDecline    OREQ      *ON
     C                   EXFMT     WDW01
      *** Beg Add ***  11/25/13  ******************************************************
      /Free
       If btnAccept = *On;
          @@AAMT = scAAmt;
          @@RTCD = 'Y';
       ElseIf btnDecline = *On;
          @@RTCD = *Blank;
       EndIf;
      /End-Free
      *** End Add ***  11/25/13  ******************************************************
      *** Beg Del ***  11/25/13  ******************************************************
     C******INKL         IFEQ      *OFF
     C******             EXSR      ERRCK
     C******             ENDIF
      *** End Del ***  11/25/13  ******************************************************
     C                   ENDDO
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*****************************************************************
     C* CHKAPP - CHECKS TO SEE IFF APPORTION APPLIES
     C*****************************************************************
     C     CHKAPP        BEGSR
     C*
     C                   SETOFF                                       90
     C                   MOVEL     Q1PGM         SCPGM
     C*
     C* Does the payment value one code have the apportionemnt flag set
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   MOVEL(P)  @@BR2         KKKEY
     C     TBKY          CHAIN     PB201                              99
     C     *IN99         IFEQ      *OFF
     C     B2PSIG        ANDEQ     'Y'
     C*
     C                   Z-ADD     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*
     C*** BEG CHG *** 09/23/03 ****************************
     C********** CMKY      CHAINPCM01                99
      *** Beg CHG ***  11/25/13  ******************************************************
     C*****CMKY          CHAIN     PCM01X                             99
     C     CMKY          CHAIN     PCM01                              99
      *** END CHG ***  11/25/13  ******************************************************
     C*** END CHG *** 09/23/03 ****************************
     C     *IN99         IFEQ      *OFF
     C     CMAEFF        ANDNE     *ZEROS
     C     CMAEFF        ANDLE     XXFRDT
     C*
     C                   SETON                                        90
     C                   EXSR      LOADSC
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
      *** Beg Del ***  11/25/13  ******************************************************
     C*****************************************************************
     C* ERRCK - ERROR CHECK
     C*****************************************************************
     C*****ERRCK         BEGSR
     C*****
     C*****              SETOFF                                       7191
     C******
     C* Apportionment Flag must be 'Y' or 'N'
     C*****
     C*****SCAPPF        IFNE      'Y'
     C*****SCAPPF        ANDNE     'N'
     C*****              SETON                                        7191
     C*****              ENDIF
     C*****
     C******IN91         IFEQ      *OFF
     C*****              Z-ADD     SCAAMT        @@AAMT
     C*****              MOVEL     SCAPPF        @@RTCD
     C*****              ENDIF
     C*****
     C*****              ENDSR
      *** End Del ***  11/25/13  ******************************************************
     C*****************************************************************
      * LOADSC - LOAD SCREEN FIELDS
     C*****************************************************************
     C     LOADSC        BEGSR
     C*
     C                   Z-ADD     @@AMT         SCAMT
     C                   MOVEL     @@BR2         SCBR2
     C                   MOVEL     B2DESC        SCBR2D
     C                   Z-ADD     CMAPCT        SCAPCT
     C     CMAPCT        DIV       100           XXAPCT            5 4
     C     SCAMT         MULT(H)   XXAPCT        SCAAMT
     C*
     C                   ENDSR
