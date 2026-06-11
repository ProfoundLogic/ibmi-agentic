     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FA720R - WCMC INFO SCREEN                   C:53 12/08/98    *
     H*                                                               *
     H*  CALLED FROM: VARIOUS PGMS                                    *
     H*  CALLS TO:                                                    *
     H*****************************************************************
     H*  DESCRIPTION: DISPLAYS A MESSAGE IN THE ARRAYS BASED ON A     *
     H*               PARM BEING PASSED IN                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/06/03 C:79 PROJECT: FS269                                 *
     H*                ADD MESSAGE PARM VALUE 6 UNPROCESSED CHECKS    *
     H*                                                               *
     H*  12/13/02 C:79 PROJECT: MC255                                 *
     H*                ADD MESSAGE PARM VALUE 4   CONTROVERTED CLAIMS *
     H*                                                               *
     H*  12/17/99 C:62 PROJECT: Y2K                                   *
     H*                -FIX F12=CANCEL                                *
     H*                                                               *
     H*  08/10/99 C:62 PROJECT: MC178                                 *
     H*                -ADD F12=CANCEL                                *
     H*                -CHECK AUTHORITY TO DETERMINE IF WORKFLOW      *
     H*                 SHOULD CONTINUE.                              *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FFA720F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEGIN ADD *** 08/10/99 **************************************
     FSMUPL     IF   E           K DISK
     F*** END   ADD *** 08/10/99 **************************************
     F*

     D MG1             S             60    DIM(7) CTDATA PERRCD(1)              MSG1
     D MG2             S             60    DIM(7) CTDATA PERRCD(1)              MSG2
     D MG3             S             60    DIM(7) CTDATA PERRCD(1)              MSG3
     D*** BEG ADD *** 12/13/02 ****************************************
     D MG4             S             60    DIM(7) CTDATA PERRCD(1)              MSG4
     D*** END ADD *** 12/13/02 ****************************************
     D*** BEG ADD *** 11/06/03 ****************************************
     D MG5             S             60    DIM(7) CTDATA PERRCD(1)              MSG5
     D*** END ADD *** 11/06/03 ****************************************
     D DTA             S              1    DIM(60)
     D*** BEGIN ADD *** 08/10/99 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** END   ADD *** 08/10/99 **************************************
     D*
     D                 DS
     D  SCMA                   1    240
     D                                     DIM(4)                               SCDATA
     D  SCMA01                 1     60
     D  SCMA02                61    120
     D  SCMA03               121    180
     D  SCMA04               181    240
     D                 DS
     D  SCMB                   1    180
     D                                     DIM(3)                               SCDATA
     D  SCMB01                 1     60
     D  SCMB02                61    120
     D  SCMB03               121    180
     D*********                             181 240 SCMB04
     D*
     D CHGNUM          C                   CONST('SA020R')
     D TITL2           C                   CONST('This is an informa-
     D                                     tion window.')

     I*

     C     *LIKE         DEFINE    FNCO#         @@CO#
     C     *LIKE         DEFINE    FNSUB#        @@SUB#
     C     *LIKE         DEFINE    FNFND         @@FND
     C     *LIKE         DEFINE    FNNAME        XXFNAM
     C     *LIKE         DEFINE    SMNAME        XXSNAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@MGID            1
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@FND
     C*** BEGIN ADD *** 08/10/99 **************************************
     C                   PARM                    @@KL              1
     C*** END   ADD *** 08/10/99 **************************************
     C*
     C     CHGPL         PLIST
     C                   PARM                    QQNUM            80
     C                   PARM                    QQMASK           10
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     FNKEY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C*
     C     SMKEY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C*** BEGIN ADD *** 08/10/99 **************************************
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** END   ADD *** 08/10/99 **************************************
     C*
     C*** BEG ADD *** 12/13/02 ****************************************
     C     @@MGID        IFEQ      '4'
     C                   SETON                                        21
     C                   ELSE
     C                   SETOFF                                       21
     C                   ENDIF
     C*** END ADD *** 12/13/02 ****************************************
     C*
     C     @@MGID        CASEQ     '1'           MSG1
     C     @@MGID        CASEQ     '2'           MSG2
     C     @@MGID        CASEQ     '3'           MSG3
     C*** BEG ADD *** 12/13/02 ****************************************
     C     @@MGID        CASEQ     '4'           MSG4
     C     @@MGID        CASEQ     '5'           MSG4
     C*** END ADD *** 12/13/02 ****************************************
     C*** BEG ADD *** 11/06/03 ****************************************
     C     @@MGID        CASEQ     '6'           MSG5
     C*** END ADD *** 11/06/03 ****************************************
     C                   ENDCS
     C*
     C                   EXFMT     SCR01
     C*** BEGIN ADD *** 08/10/99 **************************************
     C     *INKL         IFEQ      *OFF
     C*** BEG ADD *** 12/13/02 ****************************************
     C     @@MGID        ANDNE     '4'
     C     @@MGID        ANDNE     '5'
     C*** END ADD *** 12/13/02 ****************************************
     C*
     C                   EXSR      CHKAUT
     C*
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      4
     C*
     C                   MOVEL     *INKL         @@KL
     C*
     C                   ENDIF
     C*** END   ADD *** 08/10/99 **************************************
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C*=====================================================
     C     MSG1          BEGSR
     C*=====================================================
     C                   DO        7             I                 2 0
     C*
     C     I             IFLE      4
     C                   MOVEA     MG1(I)        SCMA(I)
     C                   ELSE
     C*
     C     I             SUB       4             X                 2 0
     C                   MOVEA     MG1(I)        SCMB(X)
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*
     C*=====================================================
     C     MSG2          BEGSR
     C*=====================================================
     C*
     C                   EXSR      TITLE
     C*
     C                   EXSR      NAMES
     C*
     C                   DO        7             I
     C*
     C     I             IFLE      4
     C                   MOVEA     MG2(I)        DTA(1)
     C*
     C                   SELECT
     C*
     C     I             WHENEQ    1
     C                   MOVEL(P)  @@SUB#        QQNUM
     C                   MOVEL(P)  *BLANKS       QQMASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     QQNUM         ALSUB#            5
     C                   MOVEA     ALSUB#        DTA(13)
     C                   MOVEA     XXSNAM        DTA(19)
     C     I             WHENEQ    2
     C                   MOVEL(P)  @@FND         QQNUM
     C                   MOVEL(P)  *BLANKS       QQMASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     QQNUM         ALFND             3
     C                   MOVEA     ALFND         DTA(15)
     C                   MOVEA     XXFNAM        DTA(19)
     C     I             WHENEQ    3
     C                   MOVEA     XXEXDT        DTA(13)
     C                   ENDSL
     C*
     C                   MOVEA     DTA(1)        SCMA(I)
     C                   ELSE
     C*
     C     I             SUB       4             X
     C                   MOVEA     MG2(I)        SCMB(X)
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*
     C*=====================================================
     C     MSG3          BEGSR
     C*=====================================================
     C                   DO        7             I
     C*
     C                   EXSR      TITLE
     C*
     C     I             IFLE      4
     C                   MOVEA     MG3(I)        SCMA(I)
     C                   ELSE
     C*
     C     I             SUB       4             X
     C                   MOVEA     MG3(I)        SCMB(X)
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*
     C*** BEG ADD *** 12/13/02 ****************************************
     C*=====================================================
     C     MSG4          BEGSR
     C*=====================================================
     C                   DO        7             I
     C*
     C                   EXSR      TITLE
     C*
     C     I             IFLE      4
     C                   MOVEA     MG4(I)        SCMA(I)
     C                   ELSE
     C*
     C     I             SUB       4             X
     C                   MOVEA     MG4(I)        SCMB(X)
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*** END ADD *** 12/13/02 ****************************************
     C*** BEG ADD *** 11/06/03 ****************************************
     C*=====================================================
     C     MSG5          BEGSR
     C*=====================================================
     C                   DO        7             I
     C*
     C                   EXSR      TITLE
     C*
     C     I             IFLE      4
     C                   MOVEA     MG5(I)        SCMA(I)
     C                   ELSE
     C*
     C     I             SUB       4             X
     C                   MOVEA     MG5(I)        SCMB(X)
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*** END ADD *** 11/06/03 ****************************************
     C*
     C*=====================================================
     C     NAMES         BEGSR
     C*=====================================================
     C     FNKEY         CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FNNAME        XXFNAM
     C*
     C                   Z-ADD     FNEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C                   MOVEL(P)  SRDTO         QQNUM
     C                   MOVEL(P)  'DATE'        QQMASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     QQNUM         XXEXDT           10
     C                   ENDIF
     C*
     C     SMKEY         CHAIN     PSM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     SMNAME        XXSNAM
     C                   ENDIF
     C                   ENDSR
     C*
     C*=====================================================
     C     TITLE         BEGSR
     C*=====================================================
     C                   MOVE      TITL2         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCTITL
     C                   ENDSR
     C*
     C*=====================================================
     C*** BEGIN ADD *** 08/10/99 **************************************
     C*================================================================
     C* CHKAUT - CHECK TO SEE IF USER HAS AUTHORITY TO CONTINUE.
     C*================================================================
     C     CHKAUT        BEGSR
     C*
     C                   CALL      'FA715R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@CO#         ##CO#             3 0
     C                   PARM      @@SUB#        ##SUB#            5 0
     C                   PARM      @@FND         ##FND             3 0
     C                   PARM      *ZEROS        ##PVMC            1 0          Init Med
     C                   PARM      *ZEROS        ##PVPR            1 0          Init Prov
     C                   PARM      *BLANKS       ##WCUP            1            Allw Upd WCMS Clms
     C                   PARM      *ZEROS        ##CLDL            3 0          Clm Dlt Lvl
     C                   PARM      *BLANKS       ##CLDT            1            Clm Dlt Typ
     C                   PARM      *ZEROS        ##CLTL            3 0          Clm Xfer Lvl
     C                   PARM      *BLANKS       ##CLTT            1            Clm Xfer Typ
     C                   PARM      *ZEROS        ##SDCL            3 0          Settle Dt Lvl
     C                   PARM      *BLANKS       ##SDCT            1            Settle Dt Typ
     C                   PARM      *ZEROS        ##BSCL            3 0          BWC Sts Lvl
     C                   PARM      *BLANKS       ##BSCT            1            BWC Sts Typ
     C                   PARM      *ZEROS        ##DCDL            3 0          Del Diag Cd Lvl
     C                   PARM      *BLANKS       ##DCDT            1            Del Diag Cd Typ
     C                   PARM      *ZEROS        ##DCCL            3 0          Chg Diag St Lvl
     C                   PARM      *BLANKS       ##DCCT            1            Chg Diag St Typ
     C                   PARM      *ZEROS        ##C#CL            3 0          Clt Case Lvl
     C                   PARM      *BLANKS       ##C#CT            1            Clt Case Typ
     C                   PARM      *BLANKS       ##FIMR            5            RMCD: From IMR
     C                   PARM      *BLANKS       ##TIMR            5            RMCD: To IMR
     C                   PARM      *BLANKS       ##FRN             5            RMCD: From RN
     C                   PARM      *BLANKS       ##TRN             5            RMCD: To RN
     C                   PARM      *ZEROS        ##CS#L            3 0          St Clm#  Lvl
     C                   PARM      *BLANKS       ##CS#T            1            St Clm#  Typ
     C                   PARM      *ZEROS        ##DNSL            3 0          Ovr DNYD Lvl
     C                   PARM      *BLANKS       ##DNST            1            Ovr DNYD Typ
     C                   PARM      *BLANKS       ##NERC            5            RMCD: New Emplr
     C                   PARM      *BLANKS       ##MEUS           10            New Employer
     C                   PARM      *BLANKS       ##NWPA            5            RMCD: ADD PC
     C                   PARM      *BLANKS       ##CPAA            5            RMCD: 'A' OR 'N'
     C                   PARM      *BLANKS       ##CPAD            5            RMCD: 'D'
     C                   PARM      *ZEROS        ##PLCL            3 0          LIAB CHG LVL
     C                   PARM      *BLANKS       ##PLCT            1            LIAB CHG TYP
     C*
     C     @@MGID        IFEQ      '3'
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'SA006R'
     C                   PARM      UPLVL         ##ULVL            3 0          USER LEVEL
     C                   PARM      UPTYPE        ##UTYP            1            USER TYPE
     C                   PARM      ##DNSL        ##FLVL            3 0          DNYD LEVEL
     C                   PARM      ##DNST        ##FTYP            1            DNYD TYPE
     C                   PARM      *BLANKS       ##RTCD            1            RETURN CODE
     C*
     C     ##RTCD        IFEQ      'F'
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF                                                  RTCD = 'F'
     C*
     C                   ELSE
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 08/10/99 **************************************
     O*
** MG1


      The function you have requested is not available.




** MG2
Subscriber:
Group . . :
Expired on:

The following screen(s) will be shown in display mode.
No updating is allowed.

** MG3

                This case has been denied.

        You must have the proper override authority
                to update a denied case.


** MG4

  This claim is controverted and currently will not allow
                     medical payments.

Verify the status of this claim with your claims department.


** MG5

  You are reconsidering a bill with an un-processed payment.

   It is strongly recommended that you process the previous
        payment request before reconsidering this bill.


