     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY316 ******* dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
      * *****************************************************************
      /TITLE F2013R - PROCEDURE CODE SELECTION
      * *****************************************************************
      *  Description:  THIS PROGRAM WILL DISPLAY A WINDOW TO ALLOW      *
      *                THE USER TO SELECT WHETHER TO GET THE PROCEDURE
      *                CODE FROM THE PROCEDURE CODE FILE FOR THE STATE
      *                OR THE REASONABLE & CUSTOMARY FILE.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:  U0061R01 AND F0361RFL                            *
      *                                                                 *
      *  Calls To   :  F2305R - PROCEDURE CODE SEARCH                   *
      *                F2310R - REASONABLE & CUSTOMARY SEARCH           *
      *                                                                 *
      *  Created    :   3/11/98  C:47  Project:  87MCB                  *
      *******************************************************************
      * NOTE:                                                           *
      *    @@RTCD = "E" - FAEOP RRN PASSED IN TO THIS PROGRAM.          *
      *    @@RTCD = "P" - UMPCP RRN PASSED IN TO THIS PROGRAM.          *
      *    @@RTCD = " " - UMPCP RRN PASSED IN TO THIS PROGRAM.          *
      *                                                                 *
      *******************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  02/18/20 C:079 Project: MC433                                  *
      *                 Bring up screen 2 if Part B Medicare and MC433  *
      *                                                                 *
      *  06/21/18 C:079 Project: TX101                                  *
      *                 Bring up screen 2 if Part B Medicare            *
      *                                                                 *
      *  04/04/16 C:212 Project: OK022                                  *
      *                 Screen changes and other changes                *
      *                                                                 *
      *  11/19/13 C:296 Support: 84206                                  *
      *                 Change option on physician detail screen        *
      *                 for state = 'SC'                                *
      *                                                                 *
      *  07/26/12 C:277 Project TN016                                   *
      *                 Don't show ASC selection when state is 'TN'     *
      *                                                                 *
      *  11/10/11 C:062 Support MC099 78431
      *                 Correct return info.                            *
      *                                                                 *
      *  09/02/11 C:274 Project FS392
      *                 Search the Ambulance rates for TX.              *
      *                                                                 *
      *  08/16/11 C:274 Project CT007
      *                 Search the Medicare DME rates for CT.           *
      *                                                                 *
      *  04/12/11 C:190 Project KY019
      *                 Search the ambulance file for KY.               *
      *                                                                 *
      *  10/01/10 C:198 Project WV009
      *                 More options/edits on F10 = Search
      *                 if state is based on medicare FS.
      *
      *  08/12/10 C:198 Project: 114MWS
      *                 Include additional fields when bill is Medicare
      *                 PPO processing bill.
      *
      *  11/06/00 C:62  PROJECT: FS215
      *                 ENHANCE FUNCTIONALITY TO WHERE IT CAN BE CALLED
      *                 FROM PBILLS PROGRAMS.                           *
      *                                                                 *
      *  07/19/99 C:62  PROJECT: MC208
      *                 DEFAULT PROCEDURE FLAG TO '1' UPON ENTRY.
      *                                                                 *
      *  02/16/99 C:56  PROJECT: MC152
      *                 Add adjudication state parm
      *                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
     FF2013F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      * Pre-Cert Master file
     FUMPCP     IF   E             DISK
     F                                     RENAME(PPC01:PPC0P)
     FFAEOP     IF   E             DISK
     F                                     RENAME(PEO01:PEO0P)
      * Case Master file
     FFMCML1    IF   E           K DISK
      * State Master file
     FSTSTL     IF   E           K DISK
      * Prvider Auxiliary file
     FFMP1L     IF   E           K DISK
      * Reasonable & Customary file
     FFMRCP     IF   E             DISK
      *
      * Procedure Code file
     FFMPRP     IF   E             DISK    USROPN
      *
      * Procedure Code file
     FFMPRL     IF   E           K DISK    USROPN
     F                                     RENAME(PPR01:PPR02)
      *
      * Procedure Code file
     FFMPRL1    IF   E           K DISK    USROPN
     F                                     RENAME(PPR01:PPR03)
     FFTRVP     IF   E             DISK
     FFMDEP     IF   E             DISK
     FFTLBP     IF   E             DISK
     FFMDFP     IF   E             DISK
     FFMAMP     IF   E             DISK
     FFMASP     IF   E             DISK
      *
     D QCA             S             56    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     d*** Beg Add *** 02/18/20 *********************************************
     d isMC433         s              1n   Inz(*off)
     d*** End Add *** 02/18/20 *********************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D                 DS
     D  @@CMD                  1     56
     D  @@ST                  38     39
      *
     D                 DS
     D  @@CMD2                 1     56
     D  @@ST2                 38     39
      *
     D                 DS
     D  @@CMD3                 1     56
     D  @@ST3                 40     41
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          FROM UMPCP OR FAEOP
     C                   PARM                    @@PROC            5
     C                   PARM                    @@PROD           50
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@AST             2
      *
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  PROCEDURE CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXCPCD                         ##CPT4
     C*
     C     PRKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXDSC1
      *
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
      *
     C     STKY          KLIST
     C                   KFLD                    XXAST            10
      *
     C     *LIKE         DEFINE    PRCPT4        XXCPCD                         ##CPT4
     C     *LIKE         DEFINE    PRDSC1        XXDSC1                         CPT4 DESC
     C     *LIKE         DEFINE    PCCO#         KKCO#
     C     *LIKE         DEFINE    PCSUB#        KKSUB#
     C     *LIKE         DEFINE    PCFND         KKFND
     C     *LIKE         DEFINE    PCFYR         KKFYR
     C     *LIKE         DEFINE    PCCASE        KKCASE
     C     *LIKE         DEFINE    PCFEIN        KKFEIN
     C     *LIKE         DEFINE    PCMOD         KKMOD
     C     *LIKE         DEFINE    STMFS         XXMFS
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  GET ADJUDICATION STATE
      *
     C     Q1PRMS        IFGT      6
     C     @@AST         IFNE      *BLANKS
     C                   MOVEL     @@AST         SCPRST
     C                   ENDIF
     C                   ENDIF
     C****** Beg Add ***** 04/04/16 *******************************
     C** initialize
     C/free
        // default error indicators
        erDME = *off;
        erAMB = *off;

        // default visibility
        vsRCF = *off;
        vsAMB = *off;
        vsDME = *off;

        // default top position
        lblRCFtp = 205;
        txbRCFtp = 205;
        lblAMBtp = 205;
        txbAMBtp = 205;
        lblDMEtp = 205;
        txbDMEtp = 205;

        // show RCF option as default
        vsRCF = *on;

        // If MFS = 'C', show all four options
        //*** Beg Chg *** 07/12/18 ****************************************
        //***If xxMFS = 'C';
        If xxMFS = 'C' or xxMfs = 'M' or xxMfs = 'T';
        //*** End Chg *** 07/12/18 ****************************************
           vsRCF = *on;
           vsAMB = *on;
           vsDME = *on;

           lblRCFtp = 205;
           txbRCFtp = 205;
           lblAMBtp = lblRCFtp + 40;
           txbAMBtp = txbRCFtp + 40;
           lblDMEtp = lblAMBtp + 40;
           txbDMEtp = txbAMBtp + 40;
        EndIf;

      /end-free
     C****** End Add ***** 04/04/16 *******************************
     C*
     C     @@AST         IFEQ      'KY'
     C     @@AST         OREQ      'TX'
     C****** Beg Chg ***** 04/04/16 *******************************
     C                   MOVE      *ON           vsAMB
     C                   MOVE      *OFF          vsRCF
     C                   MOVE      *OFF          vsAMB
     C                   ENDIF
     C*
     C*****              MOVE      *ON           *IN82
     C*****              ELSE
     C*****              MOVE      *OFF          *IN82
     C*****              ENDIF
     C****** End Chg ***** 04/04/16 *******************************
     C*
     C     @@AST         IFEQ      'CT'
     C****** Beg Chg ***** 04/04/16 *******************************
     C                   MOVE      *ON           vsDME
     C                   MOVE      *OFF          vsRCF
     C                   MOVE      *OFF          vsAMB
     C                   ENDIF
     C*
     C*****              MOVE      *ON           *IN83
     C*****              ELSE
     C*****              MOVE      *OFF          *IN83
     C*****              ENDIF
     C****** End Chg ***** 04/04/16 *******************************
     C*
     C                   MOVEL     Q1PGM         SCPGM
     C****** Beg Add ***** 11/19/13 *******************************
     C     @@AST         ifeq      'SC'
     C                   move      ' '           @@RTCD
     C                   endif
     C****** End Add ***** 11/19/13 *******************************
     C     @@RTCD        IFEQ      'E'
     C*
     C     @@RRN         CHAIN     PEO0P                              99
     C                   MOVEL     EOAST         SCPRST
     C                   MOVEL     EOAST         XXAST
     C     STKY          CHAIN     STSTL                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     STMFS         XXMFS
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     @@RRN         CHAIN     PPC0P                              99
     C                   Z-ADD     PCCO#         KKCO#
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   Z-ADD     PCFEIN        KKFEIN
     C                   Z-ADD     PCMOD         KKMOD
      *
     C     CMKY          CHAIN     FMCML1                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CMAST         XXAST
      *
     C     STKY          CHAIN     STSTL                              61
     C     *IN61         IFEQ      *OFF
     C     STINST        IFEQ      'Y'
     C                   MOVEL(P)  CMAST         SCPRST
     C                   END
      *
     C     STINST        IFEQ      *BLANK
     C     P1KY          CHAIN     FMP1L                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL(P)  P1ST          SCPRST
     C                   END
     C                   END
      *
     C                   END
     C                   END
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       @@RTCD
      *
      * ---------------------------------------------------------------
      *
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C                   MOVEL     '1'           SCPHYF
     C                   ENDIF
     C     *INKL         DOUEQ     *ON
      *
     C     ##CPT4        IFNE      *BLANK
     C     ##RRN         ORNE      *ZERO
     C     ##RRN1        ORNE      *ZERO
     C     ##RRN2        ORNE      *ZERO
     C                   LEAVE
     C                   END
     C                   MOVEL     '1'           SCPROF
      *
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     XXMFS         OREQ      'Y'
     C*** Beg Add *** 07/26/12 *****************************************
     C     EOAST         OREQ      'TN'
     C     @@AST         OREQ      'TN'
     C*** Beg Add *** 06/21/18 *****************************************
     C     xxMfs         Oreq      'B'
     C*** Beg Add *** 02/18/20 *****************************************
     C     isMC433       AndEq     '1'
     C*** End Add *** 02/18/20 *****************************************
     C*** Beg Add *** 06/21/18 *****************************************
      /Free
          scphyf = '1';

          Select ;
          when eoast = 'TN';
          *in84 = *on;

          when @@ast = 'TN';
          *in84 = *on;
          *in85 = *on;

          Other;
          *in84 = *off;
          *in85 = *off;
          Endsl;

      /End-Free
     C*** End Add *** 07/26/12 *****************************************
     C                   EXSR      PRCWDW
     C                   ELSE
     C                   EXFMT     WDW01
     C                   MOVEA     '000'         *IN(71)
      *
     C     *INKL         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     SCPROF        WHENEQ    '1'
     C     SCRCF         ANDEQ     '1'
     C                   MOVE      *ON           *IN71                          SELECTED BOTH
      *
     C     SCPROF        WHENNE    '1'
     C     SCPROF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN72                          INVALID CODE
      *
     C     SCRCF         WHENNE    '1'
     C     SCRCF         ANDNE     *BLANK
     C                   MOVE      *ON           *IN73                          INVALID CODE
      *
     C     SCPROF        WHENEQ    '1'
     C     SCAMB         ANDEQ     '1'
     C                   MOVE      *ON           *IN71                          SELECTED BOTH
      *
     C     SCAMB         WHENNE    '1'
     C     SCAMB         ANDNE     *BLANK
     C****** Beg Chg ***** 04/04/16 *******************************
     C                   MOVE      *ON           erAMB
     C*
     C*****              MOVE      *ON           *IN81                          INVALID CODE
     C****** End Chg ***** 04/04/16 *******************************
      *
     C     SCPROF        WHENEQ    '1'
     C     SCDME         ANDEQ     '1'
     C                   MOVE      *ON           *IN71                          SELECTED BOTH
      *
     C     SCDME         WHENNE    '1'
     C     SCDME         ANDNE     *BLANK
     C****** Beg Chg ***** 04/04/16 *******************************
     C                   MOVE      *ON           erDME
     C*
     C*****              MOVE      *ON           *IN83                          INVALID CODE
     C****** End Chg ***** 04/04/16 *******************************
      *
      * PROCEDURE CODES
      *
     C     SCPROF        WHENEQ    '1'
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRP
     C                   CLOSE     FMPRL
     C                   CLOSE     FMPRL1
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*   DELETE OVERRIDE TO FMPRP, FMPRL & FMPRL1
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH
     C*
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*   OVERRIDE TO FMPRP, FMPRL & FMPRL1
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     SCPRST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(4)        @@CMD2
     C                   MOVEL     SCPRST        @@ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD2
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(6)        @@CMD3
     C                   MOVEL     SCPRST        @@ST3
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD3
     C                   PARM                    LENGTH
     C*
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRP
     C                   OPEN      FMPRL
     C                   OPEN      FMPRL1
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCPRST        ##ST              2
     C                   PARM      *BLANKS       ##CPT4           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C                   MOVE      *BLANK        SCPROF
     C*
     C     ##CPT4        IFEQ      *BLANKS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
      *
      * REASONABLE & CUSTOMARY SEARCH
      *
     C     SCRCF         WHENEQ    '1'
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCRCF
      * AMBULANCE FILE
      *
     C     SCAMB         WHENEQ    '1'
     C                   CALL      'F2369R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN1            9 0
     C*
     C     ##RRN1        IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN1        IFNE      *ZERO
     C     ##RRN1        CHAIN     FMAMP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AMHCPC        @@PROC
     C                   MOVEL(P)  AMDESC        @@PROD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCAMB
     C*
      * Medicare DME file
      *
     C     SCDME         WHENEQ    '1'
     C                   CALL      'F2371R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN2            9 0
     C*
     C     ##RRN2        IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN2        IFNE      *ZERO
     C     ##RRN2        CHAIN     FMDEP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DEHCPC        @@PROC
     C                   MOVEL(P)  DEDESC        @@PROD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCDME
     C*
     C                   ENDSL
     C                   ENDIF
      *
      * CLEAR 'SEARCHING' MSG
     C                   CALL      'SRRMVMSG'                           99
      *
     C                   END
      *
     C                   ENDDO
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C     ##CPT4        IFEQ      *BLANK
     C     ##RRN         ANDEQ     *ZERO
     C     ##RRN1        ANDEQ     *ZERO
     C     ##RRN2        ANDEQ     *ZERO
     C                   MOVE      '1'           @@RTCD
     C                   END
      *
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C                   ELSE
     C     ##CPT4        IFNE      *BLANK
     C                   MOVEL(P)  ##CPT4        @@PROC
     C                   MOVEL(P)  ##DESC        @@PROD
     C                   END
      *
     C     ##RRN         IFNE      *ZERO
     C     @@PROC        ANDEQ     *BLANKS
     C     ##RRN         CHAIN     FMRCP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  RCCPT4        @@PROC
     C                   MOVEL(P)  RCDSC1        @@PROD
     C                   END
     C                   END
     C                   ENDIF
      *
      *  THE FOLLOWING STMT IS USED TO SATISFY THE COMPILER;
      *  FMPRP & FMPRL1 MUST BE REFERENCED BY AN INPUT SPEC.
      *  A CONDITION IS USED TO KEEP FROM ACCESSING THESE
      *  INPUT OPERATIONS, SINCE THESE FILES MAY BE CLOSED.
      *
     C*
     C     *IN99         IFEQ      *ON
     C     *IN99         ANDEQ     *OFF
     C*
     C     PRKY          SETLL     FMPRL
     C     PRKY2         SETLL     FMPRL1
     C     ##RRN         CHAIN     PPR01                              99
     C*
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C     PRCWDW        BEGSR
     C*
     C                   EXFMT     WDW02
     C*
     C                   MOVE      '0'           FLAG1
     C                   MOVEA     '000000'      *IN(74)
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   SELECT
     C*
     C     SCPHYF        WHENNE    '1'
     C     SCPHYF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN75                          INVALID CODE
      *
     C     SCDMEF        WHENNE    '1'
     C     SCDMEF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN76                          INVALID CODE
      *
     C     SCLABF        WHENNE    '1'
     C     SCLABF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN77                          INVALID CODE
      *
     C     SCDRGF        WHENNE    '1'
     C     SCDRGF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN78                          INVALID CODE
      *
     C     SCAMBF        WHENNE    '1'
     C     SCAMBF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN79                          INVALID CODE
      *
     C     SCASCF        WHENNE    '1'
     C     SCASCF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN80                          INVALID CODE
      *
     C                   ENDSL
     C*
     C                   SELECT
     C     SCPHYF        WHENNE    *BLANKS
     C     SCDMEF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1             1
     C     SCPHYF        WHENNE    *BLANKS
     C     SCLABF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCPHYF        WHENNE    *BLANKS
     C     SCDRGF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCPHYF        WHENNE    *BLANKS
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCPHYF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDMEF        WHENNE    *BLANKS
     C     SCLABF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDMEF        WHENNE    *BLANKS
     C     SCDRGF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDMEF        WHENNE    *BLANKS
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDMEF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCLABF        WHENNE    *BLANKS
     C     SCDRGF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCLABF        WHENNE    *BLANKS
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCLABF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDRGF        WHENNE    *BLANKS
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDRGF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCAMBF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C                   ENDSL
     C*
     C                   SELECT
     C*
     C     SCDMEF        WHENNE    *BLANKS
     C     SCLABF        ORNE      *BLANKS
     C     SCDRGF        ORNE      *BLANKS
     C     SCAMBF        ORNE      *BLANKS
     C     SCASCF        ORNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C     SCLABF        WHENNE    *BLANKS
     C     SCDRGF        ORNE      *BLANKS
     C     SCAMBF        ORNE      *BLANKS
     C     SCASCF        ORNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C*
     C     SCDRGF        WHENNE    *BLANKS
     C     SCAMBF        ORNE      *BLANKS
     C     SCASCF        ORNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C     SCAMBF        WHENNE    *BLANKS
     C     SCASCF        ORNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C     SCASCF        WHENNE    *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C*
     C                   ENDSL
     C*
     C     *IN74         IFEQ      '0'
     C*
     C                   SELECT
     C     SCPHYF        WHENEQ    '1'
     C                   CALL      'F2366R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     FTRVP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  RVPRCD        @@PROC
     C                   MOVEL(P)  RVDESC        @@PROD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCPHYF
     C*
     C     SCDMEF        WHENEQ    '1'
     C                   CALL      'F2371R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     FMDEP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DEHCPC        @@PROC
     C                   MOVEL(P)  DEDESC        @@PROD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCDMEF
     C*
     C     SCLABF        WHENEQ    '1'
     C                   CALL      'F2368R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     FTLBP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  LBHCPC        @@PROC
     C                   MOVEL(P)  LBDESC        @@PROD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCLABF
     C*
     C     SCDRGF        WHENEQ    '1'
     C                   CALL      'F2372R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     FMDFP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DFHCPC        @@PROC
     C                   MOVEL(P)  DFDESC        @@PROD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCDRGF
     C*
     C     SCAMBF        WHENEQ    '1'
     C                   CALL      'F2369R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     FMAMP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AMHCPC        @@PROC
     C                   MOVEL(P)  AMDESC        @@PROD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCAMBF
     C*
     C     SCASCF        WHENEQ    '1'
     C                   CALL      'F2358R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     FMASP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  ASHCPC        @@PROC
     C                   MOVEL(P)  ASDESC        @@PROD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCASCF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN74 = *OFF
     C*
     C                   ENDIF                                                  *INKL = *OFF
     C*
     C                   ENDSR
**  QCMDEXC TABLE
DLTOVR FILE(FMPRP)
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FMPRL1)
OVRDBF FILE(FMPRL1) TOFILE(*LIBL/FMPRL1  )
