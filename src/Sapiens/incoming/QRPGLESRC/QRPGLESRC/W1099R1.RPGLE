     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H/TITLE W1099R1 - Build the Selection Group Workfile for 1099s
     H** ========================================================== **
     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
     H** ========================================================== **
     H*  THIS PROGRAM WAS ORIGINALLY W1099R5 AND IS RENAMED AS       *
     H*       AS W1099R1, SO CLIENT MENUS DO NOT HAVE TO CHANGE.     *
     H** ========================================================== **
     H*  NARRATIVE: Select various Groups from Subfile, then build   *
     H*             workfile of selected groups to process for the   *
     H*             1099 Reports and IRS file. All the common FEINs  *
     H*             across the selected groups will be combined into *
     H*             into a single "B" record and reported on one     *
     H*             1099 form.                                       *
     H** ========================================================== **
     H*  CALLED FROM:                                                *
     H*         MENU                                                 *
     H*  CALLS TO:                                                   *
     H*         W1099C1 - CREATE WORKFILE AND REPORT - SUBMITTER     *
     H*                                                              *
     H*  CREATED: 09/17/02   C:132  PROJECT: WC1096                  *
     H** ========================================================== **
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  09/11/15  C:295 Project: WC2362                             *
     H*                  To Pass on to group selection process       *
     H*                                                              *
     H*  05/21/15  C:280 PHASEWARE; 33692/84412 RMLA                 *
     H*                  Remove logic of Group Restriction for clt 114
     H*                                                              *
     H*  06/04/14  C:291 Redmine #4267                               *
     H*                  Initialize Include Manual entries check box *
     H*                                                              *
     H*  12/18/13  C:290 SUPPORT: WC114 84412                        *
     H*                  Corrected the logic of Group Restriction    *
     H*                  for 1099 Process and handled if there are   *
     H*                  multiple groups restricted.                 *
     H*                                                              *
     H*  03/12/13  C:282 PROJECT 147WAL                              *
     H*                  Group Selection for 1099 Process            *
     H*                                                              *
     H*  11/19/08 C:136 PROJECT: WC1660                              *
     H*                 ADD 'CHGSTS' FUNCTION KEY TO CHANGE ALL      *
     H*                 RECORDS.                                     *
     H*                                                              *
     H*  01/11/06  C:015 Project: 135WFV                             *
     H*                  Use the COFEIN instead of F2FEIN.           *
     H*                                                              *
     H*  01/08/09  C:132 PROJECT: WC099   SEQ: 35409                 *
     H*                  If select a group to be included, Add check *
     H*                  to verify FEIN numbers same for all groups  *
     H*                  selected under the same IRS TCC code.       *
     H*                  Change Selection option from 1 to 20.       *
     H*                  Change Submit option from F6 to F7          *
     H*                                                              *
     H*  11/18/02  C:132 PROJECT: WC1096                             *
     H*                  If select a group to be included, then chg  *
     H*                  to remove from "Include" to "Exclude" pgm   *
     H*                  does not change back to "Exclude"           *
     H*                                                              *
     H** ========================================================== **
     FW1099F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     INFDS(FIDS01)
     FWXT5P     O    E             DISK    USROPN
     F*
     FWDF2L     IF   E           K DISK
     F*  Physical file definition (for refreshing subfile record)
     FWMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PFN01:DBRCDF)
     FSMCOL     IF   E           K DISK
      ***** BEG ADD **** 03/12/13 *************************************
     /copy sPrRuncmd
     /copy sPrMessage
     /copy sPrWrkwth
     /copy sPrString
     D****** Beg Add ***** 09/11/15 **********************************
     D GroupSelect     pr                  extpgm('W7944C')
     D  Program                      10    const
     D****** End Add ***** 09/11/15 **********************************
      ***** End ADD **** 03/12/13 *************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(7) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(7) ALT(OF)
      ***** BEG ADD **** 03/12/13 *************************************
     d charUser        s             10
     d charAppl        s              1
     d charRtcd        s              1
     d charAuth        s              1
     d charGrp         s              3  0
      ***** End ADD **** 03/12/13 *************************************
      ********* Beg Add ********** 12/18/13 *****************************
     d count           s              3P 0
     d FndAry          s              3P 0 dim(999)
      ********* End Add ********** 12/18/13 *****************************
     D*
     D*
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
      ***** BEG ADD **** 03/12/13 *************************************
     dRtvGrpAuth       pr                  Extpgm('SA008R')
     d ##co#                          3  0 const
     d charUser                      10    const
     d charAppl                       1    const
     d charRtcd                       1
     d charAuth                       1          options(*nopass)
     d charGrp                        3  0 const options(*nopass)
      ***** End ADD **** 03/12/13 *************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Groups-Select 1099 T-
     D                                     rans IDs')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(999)
      *
     D CLRF            C                   CONST('CLRPFM WXT5P')
      *================================================================
      *  PLIST definitions
      *================================================================

     C*
     C* The original program had input parms. This version does not use
     C* any parms, but left in code because don't want to change any
     C* client menus
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    @@CALL            1
     C                   MOVE      *BLANKS       @@CALL            1
      *
      ***** Beg Add **** 09/11/15 *************************************
      * Call to Group select program to pass-through
      /Free
         GroupSelect('1099RFILE');

        Return;
      /End-Free
      ***** End Add **** 09/11/15 *************************************
      * Set up Tax year for Second Screen
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      DSDATE        SCYR
     C                   SUB       1             SCYR
     C                   Z-ADD     SCYR          SVYR
     C                   Z-ADD     SCYR          SVSCYR
     C*
      ********* Beg Add ********** 06/04/14 *****************************
      * Initialize check box
     C                   Eval      SCINCL = 'N'
      ********* End Add ********** 06/04/14 *****************************
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
      ********* Beg Add ********** 12/18/13 *****************************
     C                   PARM                    ##XCO#            1
     C                   PARM                    ##XFND            1
      ********* End Add ********** 12/18/13 *****************************
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *================================================================
      *  KLIST definitions
      *================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
      *
     C     F2KY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
      *
     C     COKY          KLIST
     C                   KFLD                    ##CO#
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    SCYR          SVYR
     C     *LIKE         DEFINE    SCYR          SVSCYR
      *
     C/COPY SCOPYWW11C
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCKC - Errors on subfile control record
      *================================================================
     C     ERRCKC        BEGSR
      *
B01  C     *IN90         IFEQ      *ON
     C                   SETON                                        89
E01  C                   ENDIF
      *
      *  Check all transmittal codes to be the same
      *
B01  C     *IN90         IFEQ      *OFF
     C     *INKG         ANDEQ     *ON
     C                   EXSR      XCHKTC
E01  C                   ENDIF
     C                   ENDSR
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
B01  C     SCSTAT        IFEQ      'Exclude'
B02  C     SCTCC         IFEQ      *BLANKS
     C                   SETON                                        7190
E02  C                   ENDIF
E01  C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   ENDSR
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKFND
     C                   ENDSR
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
     C                   MOVE      SCPST1        KKFND
     C                   ENDSR
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      SCFND         KKFND
     C                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNDS
     C                   MOVE      *BLANKS       SCTCC
      *
      *  Chain to group aux to check for Transmitter code
      *
     C     F2KY          CHAIN     WDF2L                              99
B01  C     *IN99         IFEQ      *OFF
     C                   MOVE      F2TCC         SCTCC
     C                   MOVE      F2FEIN        SCFEIN
     C     ##CLT         IFEQ      '135'
     C                   MOVE      COFEIN        SCFEIN
     C                   ENDIF
E01  C                   ENDIF
     C                   MOVE      *ZEROS        SCMCD
      *
      *  Check to Exclude or Include in Subfile
      *
     C     SCSEL         IFNE      '20'
     C                   MOVE      'Exclude'     SCSTAT
     C                   ENDIF
      *
B02  C     SCSTAT        IFEQ      'Include'
B03  C     SCTCC         IFEQ      *BLANKS
     C                   MOVE      'Exclude'     SCSTAT
E03  C                   ENDIF
E02  C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   EXSR      ERRCKC
B01  C     *IN90         IFEQ      *OFF
      *
B02  C     *INKG         IFEQ      *ON
      *
      *  Clear WXT5P
      *
     C                   CALL      'QCMDEXC'                            90
     C                   PARM      CLRF          ##CMD
     C                   PARM      12            ##CMDL
      *
B02  C     *IN90         IFEQ      *ON
     C                   SETON                                        88
X02  C                   ELSE
      *
      *  Open WXT5P, write records from subfile
      *
     C                   MOVE      *BLANKS       ZZFND             3
     C                   OPEN      WXT5P
B03  C     1             DO        LASTRR        IX                5 0
     C     IX            CHAIN     SCR01S                             99
B04  C     *IN99         IFEQ      *OFF
B05  C     SCSTAT        IFEQ      'Include'
     C                   MOVEL     ##CO#         T5CO#
     C                   MOVEL     SCFND         T5FND
     C                   MOVEL     SCTCC         T5TCC
     C                   MOVEL     SCFEIN        T5FEIN
      * Save the first Group number for submit job parm
     C     ZZFND         IFEQ      *BLANKS
     C                   MOVE      SCFND         ZZFND
     C                   ENDIF
     C                   WRITE     PT501
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDDO
      *
     C                   CLOSE     WXT5P
      *
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
      * _______________________________________________________________ *
      *
      * Display The Screen for Parameters to Submit with W1099C1
      * then Call program to submit job to batch
      *
     C     STRT          TAG
      *
     C                   EXFMT     SCR02
      *
     C     *INKC         CABEQ     *ON           ENDFKS                   LR
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      *ON
      *
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
      *
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
      *
     C                   MOVE      XXRECF        Q2RECF
      *
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN10=*ON
      *
      *  VALIDATE THE POLICY YEAR ENTERED.
      *
     C     SCYR          CABLE     *ZERO         STRT                   2020
      *
     C     SCYR          IFNE      SVYR
     C     SCYR          ANDNE     SVSCYR
      *
     C                   CALL      'WA033R'                             99
      *
     C                   SETON                                        72
     C                   Z-ADD     SCYR          SVSCYR
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCYR<>SVYR
      *
      *  VALIDATE  COMMISSIONS (P/A/B)
      *
     C     SCCOMM        IFNE      'P'
     C     SCCOMM        ANDNE     'A'
     C     SCCOMM        ANDNE     'B'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCOMM<>P/A/B
      *
     C     SCINCL        IFNE      'Y'
     C     SCINCL        ANDNE     'N'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF                                                  SCINCL<>Y/N
      *
      *  VALIDATE PRINT OPTION FLAG
      *
     C     SCPOFL        IFNE      'P'
     C     SCPOFL        ANDNE     'L'
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPOFL<>P/L
      *
      * _______________________________________________________________ *
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      ZZFND         QQFND
     C                   MOVE      SCYR          QQYR
      *
     C                   CALL      'W1099C1'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQYR              4
     C                   PARM      SCCOMM        QQCOMM            1
     C                   PARM      ##OUTQ        QQOUTQ           10
     C                   PARM      'D'           QQMODE            1
     C                   PARM      SCPOFL        QQPOFL            1
     C                   PARM      SCINCL        QQINCL            1
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      * _______________________________________________________________ *
      *
     C                   ELSE
      *
     C                   SETON                                        90
      *
     C                   ENDIF                                                  *INKC=*OFF=*INKL
      *
E02  C                   ENDIF
E01  C                   ENDIF
E01  C                   ENDIF
      *
     C     ENDFKS        TAG
      *
     C     *INKU         IFEQ      *ON
     C     1             DO        LASTRR        IX                5 0
     C     IX            CHAIN     SCR01S                             99
     C     *IN99         IFEQ      *OFF
     C                   SELECT
     C     SCSTAT        WHENEQ    'Include'
     C                   MOVEL     'Exclude'     SCSTAT
     C     SCSTAT        WHENEQ    'Exclude'
     C                   MOVEL     'Include'     SCSTAT
     C                   ENDSL
     C                   ENDIF
     C                   UPDATE    SCR01S
     C                   ENDDO
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
B01  C     SCSEL         IFEQ      '20'
B02  C                   SELECT
S01  C     SCSTAT        WHENEQ    'Include'
     C                   MOVE      'Exclude'     SCSTAT
S02  C     SCSTAT        WHENEQ    'Exclude'
     C                   MOVE      'Include'     SCSTAT
E02  C                   ENDSL
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        ##DTIN
     C                   CALL      'SRPAST8'
     C                   PARM                    ##DTIN            8 0
     C                   PARM      180           ##NDYS            5 0
     C                   PARM                    ##ERR             1
     C*
     C                   Z-ADD     ##DTIN        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CUR180            7 0
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    ##CLT             3
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
      ********* Beg Chg ********** 12/18/13 *****************************
      /Free
       //********* Beg Chg ********** 05/21/15 *****************************
       //*********** If ##CLT = '114' and ##XFND = 'Y';
           If ##XFND = 'Y';
       //****** End Chg ********** 05/21/15 *****************************
          count = 0;
          setll *start WMFNL;
          read WMFNL;
          Dow not%eof(WMFNL);
             ExSr RtvGrpAuthority;
             If charRtcd = *off;
                count = count + 1;
                FndAry(count) = Fnfnd;
             Endif;
             read WMFNL;
          Enddo;
       Endif;
      /End-Free
      ********* End Chg ********** 12/18/13 *****************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21##CO#         READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21##CO#         READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
      ********* Beg Add ********** 03/12/13 *****************************
      ********* Beg Chg ********** 12/18/13 *****************************
      /Free
       //*********** If ##CLT = '114';
       //***********    ExSr RtvGrpAuthority;
       //***********    If charRtcd = *On;
       //*********** If ##CLT = '114' and ##XFND = 'Y';
           If ##XFND = 'Y';
       //****** End Chg ********** 05/21/15 *****************************
          If %lookup(Fnfnd:FndAry) = *zero;
             meetss = *off;
          EndIf;
       EndIf;
      /End-Free
      ********* End Chg ********** 12/18/13 *****************************
      ********* End Add ********** 03/12/13 *****************************
     C                   ENDSR
     C*================================================================
     C* XCHKTC - Check transmital codes to be the same
     C*================================================================
     C     XCHKTC        BEGSR
     C     *LIKE         DEFINE    SCTCC         SVTCC
     C                   MOVE      *BLANKS       SVTCC
     C     *LIKE         DEFINE    SCFEIN        SVFEIN
     C                   MOVE      *BLANKS       SVFEIN
B01  C     1             DO        LASTRR        IX                5 0
     C     IX            CHAIN     SCR01S                             99
B02  C     *IN99         IFEQ      *OFF
B03  C     SCSTAT        IFEQ      'Include'
B04  C     SVTCC         IFEQ      *BLANKS
     C                   MOVE      SCTCC         SVTCC
     C                   MOVE      SCFEIN        SVFEIN
E04  C                   ENDIF
B04  C     SCTCC         IFNE      SVTCC
     C     SCFEIN        ORNE      SVFEIN
     C                   SETON                                        9073
     C                   MOVEL(P)  'Exclude'     SCSTAT
     C                   SETON                                        0507
     C                   Z-ADD     SFRRN1        SCRRN1
     C                   UPDATE    SCR01S
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E01  C  N90              ENDDO
     C                   ENDSR
      ********* Beg Add ********** 03/12/13 *****************************
     C*================================================================
     C* RTVGRPAUTH - Retrieve Group Authority
     C*================================================================
      /Free

        BegSr RtvGrpAuthority;
        charUser = Q1user;
        charAppl = 'W';
        charRtcd = *off;
        charGrp  = FNfnd;
        RtvGrpAuth(##co#:charUser:charAppl:charRtcd:charAuth:charGrp);

        EndSr;
      /End-Free
      ********* End Add ********** 03/12/13 *****************************
** Column headings
Opt Group Description                               Status  TransID  FEIN
** Description--------------MISN
12020=Include/Exclude       YYN
203F3=Exit                  YYN
205F5=Refresh               NNN
207F7=Submit job            YNN
213F13=Chg user parms       YYY
220F20=Chg status all       YYY
3
