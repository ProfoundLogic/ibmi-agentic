     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *
     H/TITLE W1055R - WORK WITH POLICY PAGE ENDORSEMENT FORMS
      *===============================================================*
      * Program Description:                                          *
      *   This program will display a list of Midterm Endorsements    *
      *   for a selected group.  Each Endorsement can be accessed     *
      *   for update, display or selection depending on the calling   *
      *   program.                                                    *
      *                                                               *
      * Created   : 09/07/99                                          *
      * Programmer: C:113                                             *
      * Project   : WC904                                             *
      *===============================================================*
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/20/18 C:291 Project: 133WQP                                *
     H*                Only display Waiver Endorsements in Quote mode *
     H*                                                               *
     H* 11/10/16 C:136 Project: SY371                                 *
     H*                Use extended description field                 *
     H*                                                               *
     H* 11/07/08 C:61  PROJECT WC1744                                 *
     H*              - Change to user controlled access for files     *
     H*                (wtenpwrt, wtenpupd).                          *
     H*                                                               *
     H* 11/01/07 C:184 PROJECT WC1530                                 *
     H*                Add option 22 "Approv/Excluded States"         *
     H*                                                               *
     H* 04/09/07 C:185 SUPPORT: WA126/49385                           *
     H*          C:61  Change Access to Option 21.                    *
     H*                                                               *
     H* 03/28/07 C:184 PROJECT WC1518                                 *
     H*                Add option 22 "Approved by State"              *
     H*                                                               *
     H* 11/24/04 C:61  PROJECT CM027                                  *
     H*                Change access to options 31 and 32.            *
     H*                                                               *
     H* 02/26/04 C:61  PROJECT CM027                                  *
     H*                Remove PDSPGMR access to options 31 and 32.    *
     H*                                                               *
     H* 01/20/03 C:61  PROJECT CM027                                  *
     H*                Add options (31, 32) to write to end install   *
     H*                files: WTENPwrt, WTENPupd                      *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
      *                                                               *
      * 03/21/00 C:113 PROJECT: CM025                                 *
      *          Add Opt21=Endorsement Versioning.                    *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
      * ------------------------------------------------------------- *
      *===============================================================*
     FW1055F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      ** Physical file definition (for refreshing subfile record)
     FWTENP     IF   E             DISK
     F                                     RENAME(PEN01:PFRCDF)
      *
      ** Primary logical file definition (for displaying items)
     FWTENL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEN01:DBRCDF)
      *
      ** Secondary logical file definition (for displaying items)
     FWTENL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEN01:DBRCDF2)
      *
      ** System Table File
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      *
      ** Group Master File
     FWMFNP     IF   E             DISK
      ***** BEG DEL ******* 11/07/08 **********************************
      ******BEG ADD ******* 01/20/03 **********************************
      *********
      ***Install Files
     F*****WTENPWRTUF  E           K        DISK                      A
     F*********   PEN01                             KRENAMEPENWRT
     F*****WTENPUPDUF  E           K        DISK                      A
     F*********   PEN01                             KRENAMEPENUPD
      ******END ADD ******* 01/20/03 **********************************
      ***** END DEL ******* 11/07/08 **********************************
      ***** BEG ADD ******* 11/07/08 **********************************
      *
      ** Install Files
     FWTENPWRT  UF A E           K DISK    USROPN
     F                                     RENAME(PEN01:PENWRT)
     FWTENPUPD  UF A E           K DISK    USROPN
     F                                     RENAME(PEN01:PENUPD)
      ***** END ADD ******* 11/07/08 **********************************
      *
      *
      *
      *=========================================================================
      *  Arrays & Tables
      *=========================================================================
     D***PY SCOPYWW01E
      *
      /COPY SCOPYWW01D
     D HD              S             79    DIM(2) CTDATA PERRCD(1)              Column headings
      *** BEGIN CHG *** 03/21/00 ***********************************************
     D******************* OF      1  14  1   OFD    35   Options, Function
      ***** BEG CHG ******* 01/20/03 **********************************
     D******************* OF      1  15  1   OFD    35   Options, Function
      ***** BEG CHG ******* 11/24/04 **********************************
     D*********           OF      1  17  1   OFD    35   Options, Function
      ***** BEG CHG ******* 11/01/07 **********************************
      ***** BEG CHG ******* 03/28/07 **********************************
     D******              OF      1  18  1   OFD    35   Options, Function
     D OF              S              1    DIM(19) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(19) ALT(OF)
      ***** END CHG ******* 03/28/07 **********************************
      ***** END CHG ******* 11/01/07 **********************************
      ***** END CHG ******* 11/24/04 **********************************
      ***** END CHG ******* 01/20/03 **********************************
      *** BEGIN CHG *** 03/21/00 ***********************************************
      *
      *
      *
      *=========================================================================
      *  Data structures and constants
      *=========================================================================
     D***PY SCOPYWW01I
      *
      ** View 1
     D                 DS
     D  SCDAT1                 1     47
     D  SCFRM1                 1     15
     D  SCDES1                16     47
      *
      ** View 2
     D                 DS
     D  SCDAT2                 1     47
     D  SCDES2                 1     30
     D  SCFRM2                32     47
      *
      *
      *
      *===============================================================*
      *  /COPY required constants
      *===============================================================*
     D OPTNM           C                   CONST('Policy Page Endorse-
     D                                     ment Forms')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1055R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
      *** BEGIN ADD *** 03/21/00 ***********************************************
     D VERPGM          C                   CONST('W1053R2')
      *** END   ADD *** 03/21/00 ***********************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)
      *** BEGIN ADD *** 11/01/07 ***********************************************
      *** BEGIN ADD *** 03/28/07 ***********************************************
     D APPPGM          C                   CONST('W1555R')
      *** END   ADD *** 03/28/07 ***********************************************
      *** END   ADD *** 11/01/07 ***********************************************

      *** Beg Add ***** 08/20/18 **********************************************
     D*---------------------------------------------------------------------
     D* Stand Alone Fields
     D*---------------------------------------------------------------------
     DflgEnv           S              1a
     D@@Env            S              1a
      *** End Add ***** 08/20/18 *********************************************

      *===============================================================*
      *  PLIST definitions
      *===============================================================*

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@INKC            1
     C                   PARM      *INKL         @@INKL            1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@FORM
      *** Beg Add ***** 08/20/18 *************
     C                   PARM                    @@ENV
      *** End Add ***** 08/20/18 *************
      *
      ** Add/change/display program parameter list
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##FND
      *
      ** Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *** BEGIN ADD *** 03/21/00 ***********************************************
      ** Endorsement Versioning program parameter list
     C     VERPL         PLIST
     C                   PARM      'C'           ##MODE
     C                   PARM      SCDBRR        ##RRN
      *** END   ADD *** 03/21/00 ***********************************************
      *** BEGIN ADD *** 11/01/07 ***********************************************
      *** BEGIN ADD *** 03/28/07 ***********************************************
      *
      ** CALL W1555R program parameter list
     C     WCDPL         PLIST
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      *BLANKS       ##KC
     C     *INKL         PARM      *BLANKS       ##KL
     C*********            PARM           ##MODE
     C                   PARM      SCDBRR        ##RRN                          WTENP
      *** END   ADD *** 03/28/07 ***********************************************
      *** END   ADD *** 11/01/07 ***********************************************
      *===============================================================*
      *  KLIST definitions
      *===============================================================*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFORM
      *
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKDESC
      *
      ** System Table file
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
      *
      *
      *
      *===============================================================*
      *  *LIKE DEFN statements
      *===============================================================*
      ***** BEG ADD ******* 01/20/03 **********************************
     C     *LIKE         DEFINE    ENFORM        K1FORM
      ***** END ADD ******* 01/20/03 **********************************
     C     *LIKE         DEFINE    ENCO#         KKCO#
     C     *LIKE         DEFINE    ENFND         KKFND
     C     *LIKE         DEFINE    ENFORM        KKFORM
     C     *LIKE         DEFINE    ENFORM        @@FORM
      *** beg chg ***** 11/10/16 ************************
     C******LIKE*        DEFINE    ENDESC        KKDESC
     C******LIKE*        DEFINE    ENDESC        U1PDSC
     C******LIKE*        DEFINE    ENDESC        U2PDSC
     C     *LIKE         DEFINE    ENDSC2        KKDESC
     C     *LIKE         DEFINE    ENDSC2        U1PDSC
     C     *LIKE         DEFINE    ENDSC2        U2PDSC
      *** end chg ***** 11/10/16 ************************
      *===============================================================*
      * Main
      *===============================================================*
      *
     C/COPY SCOPYWW01C
      *
      *===============================================================*
      * CHKSSM - Set indicator 11 for subset message
      *===============================================================*
     C     CHKSSM        BEGSR
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSDESC        ORNE      *BLANKS
     C     SSST          ORNE      *BLANKS
     C     SSMNAU        ORNE      *BLANKS
     C     SSPRNT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
     C                   ENDSR
      *===============================================================*
      * CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
      *===============================================================*
     C     CVTDT         BEGSR
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C                   ENDSR                                                  CVTDT
      *===============================================================*
      * ERRCK1 - Additional errors on subfile selection
      *===============================================================*
     C     ERRCK1        BEGSR
      *** BEGIN ADD *** 11/01/07 ***********************************************
      *** BEGIN ADD *** 03/28/07 ***********************************************
     C                   SETOFF                                       81
     C     SCSEL         IFEQ      '22'
     C     ENST          IFNE      *BLANKS
     C     ENFORM        OREQ      'MID-YEAR'
     C                   SETON                                        8190
     C                   ENDIF
     C                   ENDIF
      *** END   ADD *** 03/28/07 ***********************************************
      *** END   ADD *** 11/01/07 ***********************************************
      *
      ***** BEG ADD ******* 11/24/04 **********************************
      *
     C     SCSEL         IFEQ      '99'
     C                   MOVEL     Q1USER        XXTMP             3
      *
     C     Q1USER        IFEQ      'ANDERSON'
     C     XXTMP         OREQ      'PDS'
      ***** BEG CHG ******* 11/07/08 **********************************
     C     Q1USER        OREQ      'DANDERS'
      ***** END CHG ******* 11/07/08 **********************************
      *
      ***** BEG ADD ******* 04/09/07 **********************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '21'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
      ***** END ADD ******* 04/09/07 **********************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '31'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '32'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   EXSR      BLDLNS
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** END ADD ******* 11/24/04 **********************************
      *
     C                   ENDSR
      *===============================================================*
      * ERRCK2 - Error checking for the subset screen
      *===============================================================*
     C     ERRCK2        BEGSR
      *
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
      *
     C     SSST          IFNE      ' '
     C                   MOVEL(P)  SSST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        8890
     C                   ENDIF
      *
     C     SSMNAU        IFNE      ' '
     C     SSMNAU        ANDNE     'M'
     C     SSMNAU        ANDNE     'A'
     C                   SETON                                        8790
     C                   ENDIF
      *
     C     SSPRNT        IFNE      ' '
     C     SSPRNT        ANDNE     'Y'
     C     SSPRNT        ANDNE     'N'
     C                   SETON                                        8690
     C                   ENDIF
      *
     C                   ENDSR
      *===============================================================*
      * INZSST - Initialize the subset fields
      *===============================================================*
     C     INZSST        BEGSR
      *
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
      *
     C                   ENDSR
      *===============================================================*
      * KEYINZ - Initialize key values for program start
      *===============================================================*
     C     KEYINZ        BEGSR
      *
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
     C                   MOVE      *BLANKS       KKFORM
      *
     C     *IN22         WHENEQ    *ON
     C                   MOVE      *BLANKS       KKDESC
      *
     C                   ENDSL
      *
     C                   ENDSR
      *===============================================================*
      * KEYSET - Set key values when doing "position to"
      *===============================================================*
     C     KEYSET        BEGSR
      *
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     SCPST1        KKFORM
      *
     C     *IN22         WHENEQ    *ON
     C                   MOVEL     SCPST2        KKDESC
      *
     C                   ENDSL
      *
     C                   ENDSR
      *===============================================================*
      * KEYSFL - Set key values from subfile records for paging
      *===============================================================*
     C     KEYSFL        BEGSR
      *
     C                   MOVEL     SCFND         KKFND
      *
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     SCFORM        KKFORM
      *
     C     *IN22         WHENEQ    *ON
     C                   MOVEL     SCDESC        KKDESC
      *
     C                   ENDSL
      *
     C                   ENDSR
      *===============================================================*
      * KPDO - Subroutine for F15=Mass copy
      *===============================================================*
     C     KPDO          BEGSR
      *
     C                   CALL      'W1053R'
     C     *INKC         PARM                    @@INKC
     C     *INKL         PARM                    @@INKL
     C                   PARM      'C'           XXMODE            1
     C                   PARM                    @@RRN
     C                   PARM      'P'           @@PMF             1
      *
     C                   ENDSR                                                  KPDO
      *===============================================================*
      * MVDBSC - Move data base fields to screen fields
      *===============================================================*
     C     MVDBSC        BEGSR
      *
     C                   MOVEL     ENSTAT        SCSTAT
     C                   MOVEL     ENFND         SCFND
     C                   MOVEL     ENFORM        SCFORM
      *** beg chg ***** 11/10/16 ************************
     C***********        MOVEL(P)  ENDESC        SCDESC
      /Free
       scDesc = ENdsc2;
      /End-Free
      *** end chg ***** 11/10/16 ************************
     C                   MOVEL     ENST          SCST
     C                   MOVEL     ENMNAU        SCMNAU
     C                   MOVEL     ENPRNT        SCPRNT
      *
     C                   Z-ADD     ENEDDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCEDDT
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     ENFORM        SCFRM1
      *** beg chg ***** 11/10/16 *************************
     C***********        MOVEL     ENDESC        SCDES1
      /Free
       If ENdsc2 <> *blanks;
          scDes1 = EnDsc2;
       Else;
          scDes1 = EnDesc;
       Endif;
      /End-free
      *** end chg ***** 11/10/16 *************************
     C                   MOVEL     SCDAT1        SCDATA
      *
     C     *IN22         WHENEQ    *ON
     C                   MOVEL     ENFORM        SCFRM2
      *** beg chg ***** 11/10/16 *************************
     C***********        MOVEL     ENDESC        SCDES2
      /Free
       If ENdsc2 <> *blanks;
          scDes2 = EnDsc2;
       Else;
          scDes2 = EnDesc;
       Endif;
      /End-free
      *** end chg ***** 11/10/16 *************************
     C                   MOVEL     SCDAT2        SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR
      *===============================================================*
      * OPT20 - SUBROUTINE FOR OPTION 20=CHANGE STATUS
      *===============================================================*
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR
      *
      *
      *
      *** BEGIN ADD *** 03/21/00 ***********************************************
      *===============================================================*
      * OPT21 - SUBROUTINE FOR OPTION 21=END. VERSION
      *===============================================================*
     C     OPT21         BEGSR
      *
     C                   CALL      VERPGM        VERPL
      *
     C                   ENDSR
      *** END   ADD *** 03/21/00 ***********************************************
      *** BEGIN ADD *** 11/01/07 ***********************************************
      *** BEGIN ADD *** 03/28/07 ***********************************************
      *===============================================================*
      * OPT22 - APPROVED BY STATE
      *===============================================================*
     C     OPT22         BEGSR
      *
     C     *IN81         IFEQ      *OFF
     C*
     C                   CALL      'W1555R'
     C                   PARM      @@CALL        ##CALL            1
      **** BEG CHG ****** 11/01/07 *********************************************
     C*******    *INKC     PARM *BLANK    ##KC
     C                   PARM      *BLANK        ##KC
      **** END CHG ****** 11/01/07 *********************************************
     C                   PARM      *BLANK        ##KL
     C                   PARM      SCDBRR        ##RRN                          WTENP
      **** BEG DEL ****** 11/01/07 *********************************************
     C********             CALL APPPGM    WCDPL
      **** END DEL ****** 11/01/07 *********************************************
     C*
     C                   ENDIF
     C*
      *
     C                   ENDSR
      *** END   ADD *** 03/28/07 ***********************************************
      *** END   ADD *** 11/01/07 ***********************************************
      ***** BEG ADD ******* 01/20/03 **********************************
      *
      *
      *
      *===============================================================*
      * OPT31 - Update WTENPWRT
      *===============================================================*
     C     OPT31         BEGSR
      ***** BEG ADD ******* 11/07/08 **********************************
     C                   OPEN      WTENPWRT
      ***** END ADD ******* 11/07/08 **********************************
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   MOVEL(P)  ENFORM        K1FORM
     C     K1FORM        CHAIN     PENWRT                             60
      *
     C     *IN60         IFEQ      *OFF
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   UPDATE    PENWRT
      *
     C                   ELSE
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   WRITE     PENWRT
     C                   ENDIF                                                  K1FORM
      *
      ***** BEG ADD ******* 11/07/08 **********************************
     C                   CLOSE     WTENPWRT
      ***** END ADD ******* 11/07/08 **********************************
     C                   ENDSR                                                  OPT31
      *
      *
      *
      *===============================================================*
      * OPT32 - Update WTENPUPD
      *===============================================================*
     C     OPT32         BEGSR
      ***** BEG ADD ******* 11/07/08 **********************************
     C                   OPEN      WTENPUPD
      ***** END ADD ******* 11/07/08 **********************************
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   MOVEL(P)  ENFORM        K1FORM
     C     K1FORM        CHAIN     PENUPD                             60
      *
     C     *IN60         IFEQ      *OFF
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   UPDATE    PENUPD
      *
     C                   ELSE
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   WRITE     PENUPD
     C                   ENDIF                                                  K1FORM
      *
      ***** BEG ADD ******* 11/07/08 **********************************
     C                   CLOSE     WTENPUPD
      ***** END ADD ******* 11/07/08 **********************************
     C                   ENDSR                                                  OPT32
      *
      *
      *
      ***** END ADD ******* 01/20/03 **********************************
      *===============================================================*
      * OTHFKS - Process other function keys
      *===============================================================*
     C     OTHFKS        BEGSR
      *
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKP         CASEQ     *ON           KPDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
      *
     C                   ENDSR
      *===============================================================*
      * OTHOPT - Process other options
      *===============================================================*
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '20'          OPT20
      *** BEGIN ADD *** 03/21/00 ***********************************************
     C     SCSEL         CASEQ     '21'          OPT21
      *** END   ADD *** 03/21/00 ***********************************************
      *** BEGIN ADD *** 11/01/07 ***********************************************
      *** BEGIN ADD *** 03/28/07 ***********************************************
     C     SCSEL         CASEQ     '22'          OPT22
      *** END   ADD *** 03/28/07 ***********************************************
      *** END   ADD *** 11/01/07 ***********************************************
      ***** BEG ADD ******* 01/20/03 **********************************
     C     SCSEL         CASEQ     '31'          OPT31
     C     SCSEL         CASEQ     '32'          OPT32
      ***** END ADD ******* 01/20/03 **********************************
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
      *===============================================================*
      * PGMEND - End of program processing
      *===============================================================*
     C     PGMEND        BEGSR
     C                   ENDSR
      *
      *
      *
      *===============================================================*
      * PGMSTR - Start of program processing
      *===============================================================*
     C     PGMSTR        BEGSR
      *
     C                   MOVE      ##CO#         KKCO#
      /free
      //*** Beg Add ***** 08/20/18 *******************
         flgEnv = ' ';
         If %Parms > 5;
            If @@ENV <> *Blanks;
               flgEnv = @@ENV;
            Endif;
         Endif;
      //*** End Add ***** 08/20/18 ******************
      /End-free
      *
      ** Get the current group
     C     @@RRN         CHAIN     WMFNP                              60
     C                   MOVEL     FNNAME        SCNAME
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNFND         ##FND
     C                   Z-ADD     FNFND         SCFND
     C                   ELSE
      ** or default to group 999
     C                   Z-ADD     999           KKFND
     C                   Z-ADD     999           ##FND
     C                   Z-ADD     999           SCFND
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
      *
     C     @@CALL        IFEQ      '2'
     C                   MOVE      ##FND         KKFND
     C                   ENDIF
      ***** beg DEL ******* 11/24/04 **********************************
      *********
      ******BEG ADD ******* 01/20/03 **********************************
      *********
      ***Disallow unauthorised use of options
      *********
     C*********  Q1USER    IFEQ 'ANDERSON'
      ******beg del ******* 02/24/04 **********************************
     C*********  Q1USER    OREQ 'PDSPGMR'
      ******end del ******* 02/24/04 **********************************
      *********
     C*********            MOVE '1'       XXOPFN
     C*********            MOVE '31'      XXNMBR
     C*********            MOVE 'YYN'     XXMIS
     C*********            EXSR UPDOFA
      *********
     C*********            MOVE '1'       XXOPFN
     C*********            MOVE '32'      XXNMBR
     C*********            MOVE 'YYN'     XXMIS
     C*********            EXSR UPDOFA
      *********
     C*********            ENDIF
      *********
      ******END ADD ******* 01/20/03 **********************************
      ***** end DEL ******* 11/24/04 **********************************
     C                   ENDSR
      *
      *
      *
      *===============================================================*
      * READ   - Read next record from file
      *===============================================================*
     C     READ          BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY1          READE     DBRCDF                                 95
     C     *IN22         WHENEQ    *ON
     C     KEY1          READE     DBRCDF2                                95
     C                   ENDSL
      *
     C                   ENDSR
      *===============================================================*
      * READP  - Read previous record from file
      *===============================================================*
     C     READP         BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY1          READPE    DBRCDF                                 95
     C     *IN22         WHENEQ    *ON
     C     KEY1          READPE    DBRCDF2                                95
     C                   ENDSL
      *
     C                   ENDSR
      *===============================================================*
      * SETGT  - Set greater than on file
      *===============================================================*
     C     SETGT         BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY1A         SETGT     DBRCDF                             96
     C     *IN22         WHENEQ    *ON
     C     KEY1B         SETGT     DBRCDF2                            96
     C                   ENDSL
      *
     C                   ENDSR
      *===============================================================*
      * SETLL  - Set lower limits on file
      *===============================================================*
     C     SETLL         BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY1A         SETLL     DBRCDF                             96
     C     *IN22         WHENEQ    *ON
     C     KEY1B         SETLL     DBRCDF2                            96
     C                   ENDSL
      *
     C                   ENDSR
      *===============================================================*
      * SF1UPD - Update subfile record when item is deleted
      *===============================================================*
     C     SF1UPD        BEGSR
      *
     C                   MOVEL(P)  DELETD        SCDATA
      *
     C                   ENDSR
      *===============================================================*
      * SLCTRD - Move fields for selected record to parameters
      *===============================================================*
     C     SLCTRD        BEGSR
      *
     C                   MOVEL     SCFORM        @@FORM
     C                   MOVE      *INKC         @@INKC
     C                   MOVE      *INKL         @@INKL
      *
     C                   ENDSR
      *===============================================================*
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================*
     C     STRING        BEGSR
      *
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      100           STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0

     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0

     C                   ENDSR                                                  STRING
      *===============================================================*
      * SUBSET - Determine if record should be put in subfile
      *===============================================================*
     C     SUBSET        BEGSR
      *
     C     SSDESC        IFNE      *BLANKS
      *
      *** beg chg ***** 11/10/16 **********************
     C*****LO:HI         XLATE(P)  ENDESC        U1PDSC
     c                   If        ENdsc2 <> *blanks
     c     lo:hi         Xlate(P)  ENdsc2        u1pdsc
     c                   Else
     c     lo:hi         Xlate(P)  ENdesc        u1pdsc
     c                   Endif
      *** end chg ***** 11/10/16 **********************
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
      *
     C                   EXSR      STRING
      *
     C                   ENDIF                                                  SSPDSC<>*BLANKS
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     ENSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSST          IFNE      *BLANKS
     C     SSST          ANDNE     ENST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSMNAU        IFNE      *BLANKS
     C     SSMNAU        ANDNE     ENMNAU
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSPRNT        IFNE      *BLANKS
     C     SSPRNT        ANDNE     ENPRNT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      //*** Beg Add ***** 08/20/18 ********************
         If flgEnv = 'Q' and EnType <> 'Waiver';
            MeetSS = *off;
         Endif;
      //*** End Add ***** 08/20/18 ********************
      *
     C                   ENDSR
      *===============================================================*
** Column headings
Opt Form           Description                      St  Edition Dt  M/A Prt Sts
Opt Description                    Form             St  Edition Dt  M/A Prt Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
12121=Update edition        YNN
12222=Approved/Excl. States YYN
13131=Export to wtenpWRT    NNN
13232=Export to wtenpUPD    NNN
199                         YYN
203F3=Exit                  YYN
205F5=Refresh               YYN
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
215F15=Mass copy            YNN
217F17=Subset               YYY
3
