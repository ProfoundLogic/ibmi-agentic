     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H/TITLE W8077R -  PROMPT FOR EDI ACKNOWLEDGMENT PROCESSING
     H*                                                               *
     H*****************************************************************
     H*
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*
     H* REVISIONS:
     H*
     H* 07/24/18 C:053 SUPPORT 70921
     H*                Add flag to process a file without CRLF.
     H*
     H* 12/15/16 C:180 SUPPORT 44710
     H*                Verify printer file opened when trailer missing
     H*                error is printed.
     H*
     H* 05/10/16 C:180 SUPPORT 40410
     H*                Add Report Routing Support.
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/21/09 C:53  SUPPORT 68114
     H*                Supress error rpt when processing RAK1 file.
     H*
     H* 06/17/08 C:180 SUPPORT 63841
     H*                Load Acknowledgement Trailer for R3.
     H*
     H* 12/07/07 C:53  PROJECT: WC1177
     H*                R3 Acknowledgment processing.
     H*
     H* 07/24/06 C:53  PROJECT: WC1434
     H*                ROES ASP integration (RAK1)
     H*
     H* 04/27/00 C:53  SUPPORT: WC065/19768
     H*                SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H* 08/05/99  C:118  PROJECT: WC468
     H*                  Made V3 display and RPG changes.  Added
     H*                  F10=Search.
     H*
     H*****************************************************************
     FW8077F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWXXAP     IF   E           K DISK    USROPN
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FWXA1P     O  A E             DISK
     FWXHAP     O  A E             DISK
     F*** BEG ADD *** 12/07/07 *******************
     FWXA4P     O  A E             DISK
     FWMIXL     IF   E           K DISK
     F*** END ADD *** 12/07/07 *******************
     F*** BEG CHG *** 05/10/16 **********************************
     F****QSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
     F*** END CHG *** 05/10/16 **********************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*** BEG CHG *** 12/07/07 *******************
     D*XADATA      DS                           1099
     D XADATA          DS          6089
     D*** END CHG *** 12/07/07 *******************
     D HEDAK1        E DS                  EXTNAME(WXHAP)
     D DETAK1        E DS                  EXTNAME(WXA1P)
     D TRLAK1        E DS                  EXTNAME(WXT8P)
     D*** BEG ADD *** 12/07/07 *******************
     D DETAK3        E DS                  EXTNAME(WXA4P)
     I*** END ADD *** 12/07/07 *******************

     D*** BEG ADD *** 05/10/16 *******************
     D CMD1            S            200    INZ
     D i_Prt           S               N   INZ(*OFF)

      *
      * Procedure Prototypes
      *
     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D                              200
     D                               15  5 const

     D*** END ADD *** 05/10/16 *******************

     I*
     I*

     C*________________________________________________________________
     C*   Define Key lists
     C*
     C*          SMCOL - Company Master
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*                                                                   SRINIT
     C*          SMORL - Output Routing File                              SRINIT
     C*                                                                   SRINIT
     C     ORKY          KLIST                                                                 SRINI
     C                   KFLD                    Q1PGM                                         SRINI
     C                   KFLD                    XXFILE                                        SRINI
     C*
     C*          WMUPL - User Parameter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*** BEG ADD *** 12/07/07 **********************************
     C*
     C***        WMIXL - EDI Claims Control Maintenance File
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    KKTSET
     C*
     C*** END ADD *** 12/07/07 **********************************
     C*
     C*________________________________________________________________
     C*   Define PLISTS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*________________________________________________________________
     C*   Define work fields
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    *INLR         XXHDRF
     C     *LIKE         DEFINE    *INLR         XXTRLF
     C     *LIKE         DEFINE    ORFIL#        XXFILE                                        SRINI
     C     *LIKE         DEFINE    ORTITL        XXTITL                                        SRINI
     C                   MOVE      *ZEROS        XXOTDT            8 0                         SRINI
     C                   MOVE      *ZEROS        XXOTTM            6 0                         SRINI
     C                   MOVE      *ZEROS        XXDATE            8 0                         SRINI
     C                   MOVE      *BLANK        XXKEY            10
     C*** BEG ADD *** 12/07/07 *******************
     C                   MOVE      *BLANKS       KKTSET            3
     C*** END ADD *** 12/07/07 *******************
     C*
     C*===== MAINLINE =================================================
     C*
     C     STRT01        TAG
     C*
     C*   Retrieve the User parm file
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         UPCO#
     C                   END
     C*
     C*   Retrieve the Company Master
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*** BEG ADD *** 12/07/07 **********************************
     C*
     C                   MOVE      *BLANKS       KKTSET            3
     C     IXKY          CHAIN     PIX01                              60
     C*
     C*** END ADD *** 12/07/07 **********************************
     C*
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/05/99 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*   Initialize the Record Format
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCJST
     C*
     C*** BEG ADD *** 07/24/18 ***********************************
     C                   MOVE      *BLANKS       SCSPEC
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM      'EDIACKCRLF'  XXOBJ            10
     C                   PARM      '*DTAARA'     XOTYPE            7
     C                   PARM                    XXERR             1
     C*
     C     XXERR         IFEQ      *OFF
     C                   MOVE      *ON           ISSPEC            1
     C                   ELSE
     C                   MOVE      *OFF          ISSPEC
     C                   ENDIF
     C*
     C*** END ADD *** 07/24/18 ***********************************
     C*________________________________________________________________
     C*   Display all formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*________________________________________________________________
     C*   Process all command keys
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*          Command 03 = EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 08/05/99 ***********************************
     C*  JURISDICTION STATE TABLE SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     HFLD          IFEQ      'SCJST'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'JRST'        QQTBID            4
     C                   PARM      *BLANK        QQKEY            10
     C                   PARM      ' '           QQRTCD            1
     C*
     C     QQRTCD        IFEQ      'A'
     C                   GOTO      TERM
     C                   ENDIF                                                  QQRTCD='A'
     C*
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD='B'
     C*
     C     QQKEY         IFEQ      *BLANK
     C                   GOTO      STRT
     C                   ENDIF                                                  QQKEY=BLK
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCJST
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD='*'
     C*
     C                   ENDIF                                                  HFLD=SCJST
     C*
     C                   ENDIF                                                  *INKJ=ON
     C***** END   ADD **** 08/05/99 ***********************************
     C*          Help Request
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/05/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*          Command 13 = change of user parameters
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/05/99 ***********************************
     C                   GOTO      STRT01
     C                   ENDIF
     C*
     C                   ENDIF
     C*________________________________________________________________
     C*   Process Prompt
     C*
     C     SCR01T        TAG
     C*
     C*       Validate Jurisdiction State
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** BEG ADD *** 07/24/18 ***********************************
     C*
     C                   MOVE      *OFF          *IN25
     C     ISSPEC        IFEQ      *ON
     C     SCSPEC        IFNE      'Y'
     C     SCSPEC        ANDNE     ' '
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD *** 07/24/18 ***********************************
     C*________________________________________________________________
     C*  CALL THE 824 FLATFILE UPLOAD PROGRAM
     C*
     C*** BEG ADD *** 07/24/18 ***********************************
     C     SCSPEC        IFEQ      'Y'
     C                   CALL      'W8077CA'
     C                   ELSE
     C*** END ADD *** 07/24/18 ***********************************
     C                   CALL      'W8077C'
     C*** BEG ADD *** 07/24/18 ***********************************
     C                   ENDIF
     C*** END ADD *** 07/24/18 ***********************************
     C*
     C                   OPEN      WXXAP
     C*________________________________________________________________
     C*   All edits have passed.  Read the Acknowledgement File and
     C*   do the following:
     C*   1. Retrieve the first record and insure it is the
     C*      acknowledgement header.
     C*   2. Retrieve the subsequent records and write them to the
     C*      acknowledgement detail formatted file.
     C*   3. when the acknowledgement trailer is encountered, compare
     C*      the count to the detail records read and written, and
     C*      write to the load report.
     C*   4. Repeat steps 1 through 3 for each transmission set
     C*      encountered.
     C*
     C                   READ      PXA01                                  60
     C*
     C     *IN60         IFEQ      *OFF
      *** Begin Delete 05/10/16 *******************************************************
     C*
     C*          Retrieve the Report Distribution Record                  SRINIT
     C*                                                                   SRINIT
     C*                   MOVE      '0'           XXFILE                                        SRIN
     C*     ORKY          CHAIN     POR01                              99                       SRIN
     C*     *IN99         IFEQ      '0'                                                         SRIN
     C*                   MOVEL     ORTITL        XXTITL                                        SRIN
     C*                   EXCEPT    RPDIST                                                      SRIN
     C*                   ENDIF                                                                 SRIN
      *** End Delete 05/10/16 *********************************************************

      *** Begin Add 05/10/16 **********************************************************
      /Free
          Exsr Routing;
      /End-Free
      *** End   Add 05/10/16 **********************************************************
     C*                                                                   SRINIT
     C*
     C                   EXCEPT    RPHDR
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   MOVE      *ZEROS        XXRCNT            9 0
     C                   MOVE      *OFF          XXHDRF
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*          Check for the acknowledgement header.
     C*
     C     XXHDRF        IFEQ      *OFF
     C                   EXSR      SRXHDR
     C*
     C     XXHDRF        IFEQ      *OFF
     C                   EXCEPT    RPER01
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ELSE
     C                   READ      PXA01                                  60
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*          Write the acknowledgement details until trailer found.
     C*
     C                   EXSR      SRXDTL
     C*
     C     XXTRLF        IFEQ      *ON
     C*
     C                   MOVE      T8RCNT        XTRCNT            9 0
     C     XTRCNT        IFNE      XXRCNT
     C*** BEG ADD *** 04/21/09 *******************
     C     XXRAK1        ANDEQ     *OFF
     C*** END ADD *** 04/21/09 *******************
     C                   MOVE      *ON           *IN41
     C                   MOVE      'Y'           HAERRF
     C                   ELSE
     C                   MOVE      *OFF          *IN41
     C                   ENDIF
     C*
     C                   MOVE      HAOTDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXOTDT
     C                   MOVE      HAOTTM        XXOTTM
     C                   EXCEPT    RPDTL
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    RPHDR
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   MOVE      XXRCNT        HARCNT
     C                   MOVE      'P'           HASTAT
     C                   WRITE     PHA01
     C                   MOVE      *ZEROS        XXRCNT
     C                   MOVE      *OFF          XXHDRF
     C*
     C                   ENDIF
     C*
     C                   READ      PXA01                                  60
     C                   ENDDO
     C*
     C*          Last record found should be trailer.
     C*
     C*** Begin Change ***  12/15/16 *************************************************
     C***     XXTRLF        IFEQ      *OFF
     C***                   EXCEPT    RPER02
     C***                   ENDIF
      /Free
       If XXTRLF = *OFF;
          If i_Prt = *Off;
             Exsr Routing;
          Endif;
          Except RPER02;
       Endif;
      /End-Free
     C*** End   Change ***  12/15/16 *************************************************
      *** Begin Add 05/10/16 **********************************************************
      /Free
       If i_Prt;
          Close QSYSPRT;
       Endif;
      /End-Free
      *** End   Add 05/10/16 **********************************************************
     C*________________________________________________________________
     C*   End Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*________________________________________________________________
     C*   Routine - SRXHDR
     C*
     C*   Save Acknowledgement in header structure.
     C*
-BSR CSR   SRXHDR        BEGSR                                                                 SRINI
     C*                                                                   SRINIT
     CSR                 CLEAR                   HEDAK1
     CSR                 MOVEL     XADATA        HEDAK1
     CSR   HATHDR        IFEQ      'HD1'
     CSR                 MOVE      *ON           XXHDRF
     CSR                 MOVE      UPCO#         HACO#
     CSR                 MOVE      SCJST         HAJST
     CSR                 ENDIF
     C*                                                                   SRINIT
-ESR CSR   ENXHDR        ENDSR                                                                 SRINI
     C*
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*________________________________________________________________
     C*   Routine - SRXDTL
     C*
     C*   Move the acknowledgement detail into structure and write.
     C*
-BSR CSR   SRXDTL        BEGSR                                                                 SRINI
     C*                                                                   SRINIT
     CSR                 MOVE      *OFF          XXTRLF
     C*                                                                   SRINIT
     C*** BEG DEL *** 12/07/07 *******************
     C                   CLEAR                   DETAK1
     C                   MOVEL     XADATA        DETAK1
     C*** END DEL *** 12/07/07 *******************
     C*                                                                   SRINIT
     C*** BEG ADD *** 12/07/07 *******************
     C*
     C*********  IXRLS     IFEQ '3'
     C*
     C*** RELEASE 3 ACKNOWLEDGMENT FORMAT
     C*
     CSR   A1TAK1        IFEQ      'AKC'
     C*
     C*** ACTIVATE/UPDATE THE FOLLOWING CODE WHEN THE ROES RAK1
     C*** IS SENT IN THE R3 FORMAT
     C*
     C*R****     A1TAK1    OREQ 'AR1'
     C*R****     A1ACKC    ANDEQ'TE'
     C*
     CSR                 CLEAR                   DETAK3
     CSR                 MOVEL     XADATA        DETAK3
     CSR                 MOVE      UPCO#         A4CO#
     CSR                 MOVE      SCJST         A4JST
     CSR                 MOVE      HAOTDT        A4OTDT
     CSR                 MOVE      HAOTTM        A4OTTM
     CSR                 MOVE      *BLANKS       A4STAT
     CSR                 MOVE      *BLANKS       A4ERRF
     CSR                 ADD       1             XXRCNT
     CSR                 WRITE     PA401
     C*                                                                   SRINIT
     CSR                 ENDIF
     C*R **************    ENDIF
     C*** END ADD *** 12/07/07 *******************
     C*                                                                   SRINIT
     C*** BEG ADD *** 12/07/07 *******************
     C********** IXRLS     IFEQ '1'
     C*** END ADD *** 12/07/07 *******************
     C*
     CSR   A1TAK1        IFEQ      'AK1'
     C*** BEG ADD *** 07/24/06 ***********************************************
     CSR   A1TAK1        OREQ      'AR1'
     CSR   A1ACKC        ANDEQ     'TE'
     C*** END ADD *** 07/24/06 ***********************************************
     C*** BEG ADD *** 12/07/07 *******************
     C*R *****             CLEARDETAK1
     C*R *****             MOVELXADATA    DETAK1
     C*** END ADD *** 12/07/07 *******************
     CSR                 MOVE      UPCO#         A1CO#
     CSR                 MOVE      SCJST         A1JST
     CSR                 MOVE      HAOTDT        A1OTDT
     CSR                 MOVE      HAOTTM        A1OTTM
     CSR                 MOVE      *BLANKS       A1STAT
     CSR                 MOVE      *BLANKS       A1ERRF
     CSR                 ADD       1             XXRCNT
     C*                                                                   SRINIT
     C*** BEG ADD *** 04/21/09 *******************
     CSR   A1TAK1        IFEQ      'AR1'
     CSR                 MOVE      *ON           XXRAK1            1
     CSR                 ENDIF
     C*** END ADD *** 04/21/09 *******************
     C*                                                                   SRINIT
     CSR                 WRITE     PA101
     C*                                                                   SRINIT
     C*** BEG CHG *** 12/07/07 *******************
     C*R*************      ELSE
     CSR                 ENDIF
     C*** END CHG *** 12/07/07 *******************
     C*** BEG ADD *** 12/07/07 *******************
     C*R *********         ENDIF
     C*** END ADD *** 12/07/07 *******************
     C*                                                                   SRINIT
     CSR   A1TAK1        IFEQ      'TR1'
     ***** BEGIN ADD *** 06/17/08 *************************************
     CSR   A1TAK1        OREQ      'TR2'
     ***** END ADD ***** 06/17/08 *************************************
     CSR                 CLEAR                   TRLAK1
     CSR                 MOVEL     XADATA        TRLAK1
     CSR                 MOVE      *ON           XXTRLF
     CSR                 ENDIF
     C*                                                                   SRINIT
     C*** BEG DEL *** 12/07/07 *******************
     C*R*************      ENDIF
     C*** END DEL *** 12/07/07 *******************
     C*                                                                   SRINIT
-ESR CSR   ENXDTL        ENDSR                                                                 SRINI

      *** Begin Add 05/10/16 **********************************************************
      /Free
        //--------------------------------------------------------------------*
        // Subroutine Routing                                                 *
        //--------------------------------------------------------------------*
        Begsr Routing;

         // Retrieve Report Distribution Record

           XXFILE = '0';
           Chain (Q1PGM:XXFILE) SMORL;
           If %found(SMORL);
               XXTITL = ORTITL;
               CMD1 = 'OVRPRTF FILE(QSYSPRT)'                    + ' ' +
                      'PAGESIZE(66 198)'                         + ' ' +
                      'LPI(6) CPI(15) OVRFLW(60) PAGRTT(*AUTO)'  + ' ' +
                      'OUTQ(' + %TRIM(OROUTQ) + ')'              + ' ' +
                      'FORMTYPE(' + %TRIM(ORFORM) + ')'          + ' ' +
                      'COPIES(' + %EDITC(ORCOPY:'X') + ')'       + ' ' +
                      'HOLD(' + %TRIM(ORHOLD) + ')'              + ' ' +
                      'SAVE(' + %TRIM(ORSAVE) + ')';

              QCMDEXC(CMD1:%len(CMD1));
           Endif;

           Open QSYSPRT;
           i_Prt = *on;
           Except RPDIST;
        Endsr;
      /End-Free
      *** End  Add 05/10/16 **********************************************************
     OQSYSPRT   E            RPDIST         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            RPDIST         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            RPDIST         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            RPDIST         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            RPDIST         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            RPDIST         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            RPDIST         2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          E            RPDIST         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            RPDIST         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           RPDIST
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1996'
     O          E            RPDIST         1 59
     O                       #BRND1
     O          E            RPDIST         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            RPHDR          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    128
     O          E            RPHDR          2
     O                       Q1PGM               10
     O                       XXTITL              99
     O                                          123 'PAGE:'
     O                       PAGE          Z    128
     O          E            RPHDR          2
     O                                           19 'JURISDICTION STATE:'
     O                       SCJST               22
     O          E            RPHDR          2
     O                                            7 'T-SET'
     O                                           20 'SENDER FEIN'
     O                                           35 'RECEIVER FEIN'
     O                                           58 'ORIGINAL TRANSMISSION'
     O                                           64 'TEST'
     O                                           78 'RECORDS SENT'
     O                                           96 'RECORDS RECEIVED'
     O                                          104 'ERROR'
     O*
     O          E            RPDTL          1
     O                       HATSET               6
     O                       HASEIN              18
     O                       HAREIN              31
     O                       XXOTDT        Y     45
     O                       XXOTTM              55 '  :  .  '
     O                       HATPFG              62
     O                       XTRCNT        Z     78
     O                       XXRCNT        Z     96
     O                       HAERRF             102
     O          E            RPER01         1
     O                                           50 'NO HEADER FOUND'
     O          E            RPER02         1
     O                                           50 'NO TRAILER FOUND'
