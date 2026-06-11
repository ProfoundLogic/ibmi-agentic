     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE W0810R - WORK WITH CERTIFICATE HOLDERS
     H*****************************************************************
     H*  DESCRIPTION:  This program will display a list of all        *
     H*                certificate holders for a selected employer    *
     H*                policy year.  Each holder can be accessed for  *
     H*                update, display or selection depending on the  *
     H*                calling program.                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R - WORK WITH POLCIY YEARS                            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0810R02 - CERTIFICATE HOLDER INFORMATION (OPT 2/5/20)     *
     H*    W7520C   - ROLLOVER (OPT 15)                               *
     H*    W7510C   - PRINT CERTIFICATE (OPT 18)                      *
     H*    W7520C   - ROLLOVER ALL (F15)                              *
     H*    W7510C   - PRINT ALL CERTFICATES (F18)                     *
     H*    W7510C   - PRINT BLANK CERTFICATES (F19)                   *
     H*                                                               *
     H*  CREATED: 05/12/99  PROGRAMMER: C:113 PROJECT: CM045          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/12/16 C:298 Support 38383                                  *
     H*                To fix the issue of skipping names when        *
     H*                sort by names is chosen.                       *
     H*                                                               *
     H* 12/30/10 C:61  Project: WC1880 PRISS 24                       *
     H*                Change so clt093 modified version of           *
     H*                certificate holder is not used.                *
     H*                                                               *
     H* 03/09/09 C:190 Project: WC1750                                *
     H*                Add parm 'From' & 'To date' to W7520C          *
     H*                                                               *
     H* 04/17/07 C:33  Project: WC1418  WA099/58017                   *
     H*                When selecting F18=Print All for a cx'd acct,  *
     H*                 do not display the DARTS requests as they will*
     H*                 be requested on the next window.              *
     H*                                                               *
     H* 09/20/06 C:61  SUPPORT WA093 54914                            *
     H*                Change call to W7510C for clt093 modified      *
     H*                code.                                          *
     H*                                                               *
     H* 02/07/06 C:168 Project: WC1418                                *
     H*               -Added field to check if user want to resolve   *
     H*               to DARTS and to allow to enter a remark code    *
     H*                                                               *
     H* 07/06/05 C:33  Project: 135WDG                                *
     H*               -Clear cert range fields.                       *
     H*               -F12 pressed from window should not submit job. *
     H*                                                               *
     H* 06/27/05 C:156 PROJECT 135WDG                                 *
     H*                Provide ability to print a range of certs.     *
     H*                                                               *
     H* 05/19/05  C:33   Seq#: WA102/46532                            *
     H*                  Do not roll Inactive certificates.           *
     H*                                                               *
     H* 04/07/05       PROJECT: 126WDK                                *
     H*          C:61  PROJECT 126WAI                                 *
     H*                Use different logicals for WMCEP that do not   *
     H*                select by CETYPE.                              *
     H*                                                               *
     H* 04/06/04 C:132 PROJECT WC1203                                 *
     H*                Change Default View to Cert Holder Name View.  *
     H*                                                               *
     H* 09/23/02 C:61  PROJECT WC1091                                 *
     H*                Allow print certificates for cx employers.     *
     H*                                                               *
     H* 05/12/99 C:113 PROJECT: CM045                                 *
     H*                Rewrote program to remove usage for waiver of  *
     H*                subrogation.  Waiver programs are now          *
     H*                W0815R/R02.                                    *
     H*                                                               *
     H* 03/30/99 C:33  PROJECT: WC577                                 *
     H*          C:118 RESET VALID OPTION FLAG AFTER READS TO BE SURE *
     H*                CORRECT RRN'S ARE LOADED.                      *
     H*                                                               *
     H* 04/03/98 C:78  PROJECT: WC825                                 *
     H*                ALLOW ACCESS TO DARTS. F14 ADDED.              *
     H*                                                               *
     H* 02/19/98 C:33  PROJECT: WC577                                 *
     H*                BE SURE TO RELOAD PROPER RRN WHEN RETURNING    *
     H*                FROM OPTIONS                                   *
     H*                                                               *
     H* 02/12/98 C:35  PROJECT: SUPPORT                               *
     H*                CHANGE CODE SO THAT THE CANCELLATION DATE      *
     H*                DISPLAYED ON THE SCREEN IS THE CX DATE         *
     H*                ASSOCIATED WITH THE MOST RECENT EFFECTIVE      *
     H*                DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED   *
     H*                SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY  *
     H*                DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER     *
     H*                IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF    *
     H*                THIS FIELD WITH AN INDICATOR SO THAT IF THE    *
     H*                EMPLOYER IS NOT CANCELLED, THE FIELD AND       *
     H*                HEADING WILL NOT SHOW ON THE SCREEN.           *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  30 - Called as a search                                      *
     H* N30 - Called as Maintenance                                   *
     H*  40 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0810F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMCEP     IF   E             DISK
     F                                     RENAME(PCE01:PFRCDF)
      ***  BEGIN CHG  ***  04/07/05 ***********************************
     F********WMCEL   IF  E           K        DISK
     FWMCEL6    IF   E           K DISK
      ***  END   CHG  ***  04/07/05 ***********************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCE01:DBRCDF)
      ***  BEGIN CHG  ***  04/07/05 ***********************************
     F*******WMCEL2  IF  E           K        DISK
     FWMCEL7    IF   E           K DISK
      ***  END   CHG  ***  04/07/05 ***********************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCE01:DBRCDF2)
     FWDELP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      ***  BEGIN ADD  ***  02/07/06 ***********************************
     FWMEXL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
      ***  END   ADD  ***  02/07/06 ***********************************
      /COPY SCOPYWW01D
     D*================================================================
     D*  Array & Table definitions
     D*================================================================
     D HD              S             78    DIM(10) CTDATA PERRCD(1)             HEADERS
     D OF              S              1    DIM(19) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(19) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*================================================================
     D*  Data Structures & constants definitions
     D*================================================================
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('Certificate Holders')
     D DELETD          C                   CONST('*Deleted')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D ACDPGM          C                   CONST('W0810R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CHGNUM          C                   CONST('WA003R')
     D PRINTD          C                   CONST('WA007R')
     D BATCH           C                   CONST('WA008R')
     D USPAPI          C                   CONST('W0006R')
     D SF1PAG          C                   CONST(8)
     D VIEWMX          C                   CONST(2)
     D INFOMX          C                   CONST(5)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  SCDATA                 1     74
     D  XXDATA                 1     37
     D  XXREM                 40     66
     D  CERLFL                69     69
     D  CESTAT                73     73
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  REM                    1    300
     D                                     DIM(5)
     D  CEREM1                 1     60
     D  CEREM2                61    120
     D  CEREM3               121    180
     D  CEREM4               181    240
     D  CEREM5               241    300
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40

     C*================================================================
     C*  *LIKE DEFN definitions
     C*================================================================
      ***** BEG ADD ******* 09/23/02 **********************************
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
      ***** END ADD ******* 09/23/02 **********************************
     C     *LIKE         DEFINE    CECER#        KKCER#
     C     *LIKE         DEFINE    CENAME        KKNAME
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    SCCXDT        SYSDT1
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    SCFYR         XXNFYR
     C     *LIKE         DEFINE    CETYPE        @@TYPE
     C     *LIKE         DEFINE    CETYPE        ##TYPE
     C     *LIKE         DEFINE    CETYPE        XXTYPE
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      ***** Beg Add ***** 02/12/16 ************************************
     C     *LIKE         DEFINE    CECER#        svCER#
      ***** End Add ***** 02/12/16 ************************************
     C                   Z-ADD     0             I1                3 0
     C*================================================================
     C*  PLIST Definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@VIEW            1 0
     C*
     C*  NOTE:  @@RRN is used for returning the RRN of WMCEP for
     C*         selection.
     C*         @@RRN2 will need to be passed in as the RRN of WDELP.
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C*  Change Numeric to Alpha program parameter list
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     BTCHPL        PLIST
     C     *INKL         PARM                    ##KL              1
     C*
     C     ROLLPL        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQCER#            5
     C                   PARM                    QQNFYR            3
      ***** BEG ADD ******* 03/09/09 **********************************
     C                   PARM                    QQFTDT            8
     C                   PARM                    QQTTDT            8
      ***** END ADD ******* 03/09/09 **********************************
     C*
     C     CERTPL        PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQCER#
      ***** BEG ADD ******* 06/27/05 **********************************
     C                   PARM                    QQCERT            5
      ***** END ADD ******* 06/27/05 **********************************
     C                   PARM                    QQBLNK            1
     C                   PARM                    QQRDAT            8
     C                   PARM                    QQCOPY            3
     C                   PARM                    UPOUTQ
      ***** BEG ADD ******* 02/07/06 **********************************
     C                   PARM                    QQRSFL            1
     C                   PARM                    QQRMCD            5
      ***** END ADD ******* 02/07/06 **********************************
      ***** BEG ADD ******* 09/20/06 **********************************
      *
     C     CERT93        PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQCER#
     C                   PARM                    QQBLNK            1
     C                   PARM                    QQRDAT            8
     C                   PARM                    QQCOPY            3
     C                   PARM                    UPOUTQ
      *
      ***** END ADD ******* 09/20/06 **********************************
     C*================================================================
     C*  KLIST Definitions
     C*================================================================
     C*
     C*  EMPLOYER MASTER
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKCER#
     C*
     C     KEY1B         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKNAME
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '15'
     C*
     C     SCROLL        IFEQ      'N'
     C***  Begin Add  ***  05/19/05  **************************************
     C     SCSTAT        OREQ      'I'
     C***  End   Add  ***  05/19/05  **************************************
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCROLL=N
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKCER#
     C                   MOVE      *LOVAL        KKNAME
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        KKCER#
     C                   MOVE      SCPST2        KKNAME
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCCER#        KKCER#
     C                   MOVEL     SCNAM1        KKNAME
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*===============================================================
     C* KPDO - SUBROUTINE FOR F15
     C*===============================================================
     C*
     C*  F15=ROLLOVER ALL
     C*
     C     KPDO          BEGSR
     C*
     C     SCFYR         ADD       1             XXNFYR
     C                   MOVE      XXNFYR        QQNFYR
     C*
     C                   CALL      BATCH         BTCHPL                 99
     C*
     C  NKL              CALL      'W7520C'      ROLLPL                 99
     C*
     C                   ENDSR                                                  KPDO
     C*
     C*===============================================================
     C* KSDO - SUBROUTINE FOR F18
     C*===============================================================
     C*
     C*  F18=PRINT ALL CERTIFICATES
     C*
     C     KSDO          BEGSR
     C*
     C***  Begin Add  ***  04/17/07  **************************************
     C                   MOVE      *ON           *IN18
     C***  End   Add  ***  04/17/07  **************************************
      ***** BEG ADD ******* 09/23/02 **********************************
      *
     C     XXCXDT        IFNE      *ZERO
     C     XXCXDT        ANDLE     SYSDT
     C                   EXFMT     WDW02
     C                   ENDIF
      *
     C     *INKL         IFEQ      *OFF
      ***** END ADD ******* 09/23/02 **********************************
     C                   MOVE      SYSDT1        QQRDAT
     C                   MOVE      '0'           QQCOPY
     C                   MOVE      'N'           QQBLNK
      ***** BEG ADD ******* 06/27/05 **********************************
     C                   MOVE      '1'           *IN90
     C***  Begin Add  ***  07/06/05  **************************************
     C                   CLEAR                   SCCERF
     C                   CLEAR                   SCCERT
     C***  End   Add  ***  07/06/05  **************************************
     C***  Begin Add  ***  02/07/06  **************************************
      *
     C                   MOVEL     'N'           SCRSFL
     C                   CLEAR                   SCRMCD
      *
      * Checks if there is a default remark code in this file
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C     EXCICD        ANDNE     *BLANKS
     C                   MOVEL     'Y'           SCRSFL
     C                   MOVEL(P)  EXCICD        SCRMCD
     C                   ENDIF
      *
     C                   MOVEL     SCRSFL        #RSFL             1
      *
     C***  End   Add  ***  02/07/06  **************************************
      *
     C     *IN90         DOUEQ     *OFF
     C                   EXFMT     WDW03
     C                   MOVE      '0'           *IN90
     C***  Begin Chg  ***  07/06/05  **************************************
     C********** SCCERF    IFGT SCCERT
     C  NKLSCCERF        IFGT      SCCERT
     C***  End   Chg  ***  07/06/05  **************************************
     C                   SETON                                        72  90
     C                   ENDIF
      *
     C***  BEGIN ADD  ***  02/07/06  **************************************
      *
      * Go to the SRRSFL subroutine o validate/retrieve the remark code
     C  NKL              EXSR      SRRSFL
      *
     C***  END   add  ***  02/07/06  **************************************
      *
     C                   ENDDO
     C*
     C***  Begin Add  ***  07/06/05  **************************************
     C     *INKL         IFEQ      *OFF
     C***  End   Add  ***  07/06/05  **************************************
     C*
     C                   MOVE      SCCERF        QQCER#
     C                   MOVE      SCCERT        QQCERT
      ***** BEG ADD ******* 06/27/05 **********************************
     C***  BEG   ADD  ***  02/07/06  **************************************
     C                   MOVE      SCRSFL        QQRSFL
     C                   MOVE      SCRMCD        QQRMCD
      ***** END ADD ******* 02/07/06 **********************************
     C*
      ***** BEG ADD ******* 09/20/06 **********************************
      ***** BEG del ******* 12/30/10 **********************************
     C*********  CLTID     IFEQ '093'
     C*********            CALL 'W7510C'  CERT93       99
     C*********            ELSE
      ***** END del ******* 12/30/10 **********************************
      ***** END ADD ******* 09/20/06 **********************************
     C                   CALL      'W7510C'      CERTPL                 99
      ***** BEG ADD ******* 09/20/06 **********************************
      ***** BEG del ******* 12/30/10 **********************************
     C*********            ENDIF
      ***** END del ******* 12/30/10 **********************************
      ***** END ADD ******* 09/20/06 **********************************
     C*
     C                   CALL      PRINTD                               99
      ***** BEG ADD ******* 09/23/02 **********************************
     C*
     C***  Begin Add  ***  07/06/05  **************************************
     C                   ENDIF
     C***  End   Add  ***  07/06/05  **************************************
     C*
     C                   ENDIF
      *
     C   KL              SETOFF                                       KL
      ***** END ADD ******* 09/23/02 **********************************
     C*
     C***  Begin Add  ***  04/17/07  **************************************
     C                   MOVE      *OFF          *IN18
     C***  End   Add  ***  04/17/07  **************************************
     C*
     C                   ENDSR                                                  KSDO
     C*
     C*===============================================================
     C* KTDO - SUBROUTINE FOR F19
     C*===============================================================
     C*
     C*  F19=PRINT BLANK CERTIFICATES
     C*
     C     KTDO          BEGSR
     C*
     C                   MOVE      *OFF          *IN27
     C                   MOVE      *ON           *IN90
     C                   Z-ADD     1             SCCOPY
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C*  VALIDATE THE NUMBER OF COPIES
     C*
     C  NKLSCCOPY        IFLT      1
     C     SCCOPY        ORGT      99
     C                   MOVE      *ON           *IN27
     C                   ELSE
     C                   MOVE      *OFF          *IN90
     C                   ENDIF                                                  1<SCCOPY<99
     C*
     C   KL              MOVE      *OFF          *IN90
     C*
     C                   ENDDO                                                  *IN90=*OFF
     C*
     C                   MOVE      SYSDT1        QQRDAT
     C                   MOVE      SCCOPY        QQCOPY
     C                   MOVE      'Y'           QQBLNK
     C*
      ***** BEG ADD ******* 09/20/06 **********************************
      ***** BEG del ******* 12/30/10 **********************************
     C*********  CLTID     IFEQ '093'
     C**NKL****            CALL 'W7510C'  CERT93       99
     C*********            ELSE
      ***** END del ******* 12/30/10 **********************************
      ***** END ADD ******* 09/20/06 **********************************
     C  NKL              CALL      'W7510C'      CERTPL                 99
      ***** BEG ADD ******* 09/20/06 **********************************
      ***** BEG del ******* 12/30/10 **********************************
     C*********            ENDIF
      ***** END del ******* 12/30/10 **********************************
      ***** END ADD ******* 09/20/06 **********************************
     C*
     C  NKL              CALL      PRINTD                               99
     C*
     C                   MOVE      *OFF          *INKL
     C*
     C                   ENDSR                                                  KTDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CECER#        SCCER#
     C                   MOVEL     CENAME        SCNAM1
     C                   MOVEL     CERLFL        SCROLL
     C                   MOVEL     CESTAT        SCSTAT
     C*
     C                   Z-ADD     INFO#         X                 3 0
     C                   MOVEL     REM(X)        XXREM
     C*
     C                   MOVEL(P)  CECER#        XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XXCER#            5
     C*
     C                   MOVEL(P)  CENAME        XXNAM1           30
     C*
     C                   CLEAR                   XXDATA
     C*
     C                   SELECT
     C*
     C     VIEW#         WHENEQ    1
     C*
     C                   MOVEL     XXCER#        XXDATA
     C                   MOVE      XXNAM1        XXDATA
     C*
     C     VIEW#         WHENEQ    2
     C*
     C                   MOVEL     XXNAM1        XXDATA
     C                   MOVE      XXCER#        XXDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT15 - SUBROUTINE FOR OPTION 15
     C*===============================================================
     C*
     C* OPTION 15=ROLLOVER
     C*
     C     OPT15         BEGSR
     C*
     C     SCFYR         ADD       1             XXNFYR
     C                   MOVE      XXNFYR        QQNFYR
     C*
     C                   CALL      BATCH         BTCHPL                 99
     C*
     C  NKL              CALL      'W7520C'      ROLLPL                 99
     C*
     C                   ENDSR                                                  OPT15
     C*
     C*===============================================================
     C* OPT18 - SUBROUTINE FOR OPTION 18
     C*===============================================================
     C*
     C* OPTION 18=PRINT CERTIFICATE
     C*
     C     OPT18         BEGSR
      *
     C***  Begin Add  ***  02/07/06  **************************************
      *
     C                   MOVEL     'N'           SCRSFL
     C                   CLEAR                   SCRMCD
      *
      * Checks if there is a default remark code in this file
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C     EXCICD        ANDNE     *BLANKS
     C                   MOVEL     'Y'           SCRSFL
     C                   MOVEL(P)  EXCICD        SCRMCD
     C                   ENDIF
      *
     C                   MOVEL     SCRSFL        #RSFL             1
      *
     C***  End   Add  ***  02/07/06  **************************************
      ***** BEG CHG ******* 02/07/06 **********************************
      *
     C     XXCXDT        IFNE      *ZERO
     C     XXCXDT        ANDLE     SYSDT
     C     *IN90         DOUEQ     *OFF
     C                   EXFMT     WDW02
     C                   MOVE      '0'           *IN90
      *
      * Go to the SRRSFL subroutine o validate/retrieve the remark code
      *
     C  NKL              EXSR      SRRSFL
     C                   ENDDO
      *
      * ...Record is NOT cancelled, display window for user to enter a
      *    remark code to be used.
      *
     C                   ELSE
     C     *IN90         DOUEQ     *OFF
     C                   EXFMT     WDW04
     C                   MOVE      '0'           *IN90
      *
      * Go to the SRRSFL subroutine o validate/retrieve the remark code
      *
     C  NKL              EXSR      SRRSFL
     C                   ENDDO
      ***** END CHG ******* 02/07/06 **********************************
      *
     C                   ENDIF
      *
     C     *INKL         IFEQ      *OFF
      ***** END ADD ******* 09/23/02 **********************************
     C                   MOVE      SYSDT1        QQRDAT
     C                   MOVE      '0'           QQCOPY
     C                   MOVE      'N'           QQBLNK
     C***  BEG   ADD  ***  02/07/06  **************************************
     C                   MOVE      SCRSFL        QQRSFL
     C                   MOVE      SCRMCD        QQRMCD
      ***** END ADD ******* 02/07/06 **********************************
     C*
      ***** BEG ADD ******* 09/20/06 **********************************
      ***** BEG del ******* 12/30/10 **********************************
     C*********  CLTID     IFEQ '093'
     C*********            CALL 'W7510C'  CERT93       99
     C*********            ELSE
      ***** END del ******* 12/30/10 **********************************
      ***** END ADD ******* 09/20/06 **********************************
     C                   CALL      'W7510C'      CERTPL                 99
      ***** BEG ADD ******* 09/20/06 **********************************
      ***** BEG del ******* 12/30/10 **********************************
     C*********            ENDIF
      ***** END del ******* 12/30/10 **********************************
      ***** END ADD ******* 09/20/06 **********************************
     C*
     C                   CALL      PRINTD                               99
      ***** BEG ADD ******* 09/23/02 **********************************
     C                   ENDIF
      *
     C   KL              SETOFF                                       KL
      ***** END ADD ******* 09/23/02 **********************************
     C*
     C                   ENDSR                                                  OPT18
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      ELFND         QQFND
     C                   MOVE      ELFYR         QQFYR
     C                   MOVE      ELEMP#        QQEMP#
     C                   MOVE      ELDIV         QQDIV
     C                   MOVE      *ZEROS        QQCER#
     C*
     C     *INKN         CASEQ     *ON           KNDO
     C     *INKP         CASEQ     *ON           KPDO
     C     *INKS         CASEQ     *ON           KSDO
     C     *INKT         CASEQ     *ON           KTDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      ELFND         QQFND
     C                   MOVE      ELFYR         QQFYR
     C                   MOVE      ELEMP#        QQEMP#
     C                   MOVE      ELDIV         QQDIV
     C                   MOVE      SCCER#        QQCER#
     C*
     C     SCSEL         CASEQ     '15'          OPT15
     C     SCSEL         CASEQ     '18'          OPT18
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
      ***** BEG ADD ******* 09/20/06 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      ***** END ADD ******* 09/20/06 **********************************
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN2        CHAIN     PEL01                              60
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
     C*
     C*  Get most recent cancellation date, if any
     C*
      ***** BEG ADD ******* 09/23/02 **********************************
     C                   Z-ADD     *ZERO         XXCXDT
      ***** END ADD ******* 09/23/02 **********************************
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
      ***** BEG ADD ******* 09/23/02 **********************************
     C                   Z-ADD     CX(I1)        XXCXDT
      ***** END ADD ******* 09/23/02 **********************************
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C*  GET FUND MASTER INFORMATION
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C     FNEXDT        IFNE      *ZEROS
     C     SYSDT         ANDGE     FNEXDT
     C                   MOVE      *ON           *IN09
     C                   MOVEL     '1'           @@CALL
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C                   Z-ADD     SYSDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SYSDT1
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  GET USER PARM INFORMATION
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C*  START IN VIEW THAT WAS SPECIFIED IN PARMS OR DEFAULT TO 1
     C*
      ***** BEG ADD ***** 04/06/04 **************************************
      * OVERRIDE VIEW TO START WITH NAME ORDER
     C     VIEW#         IFEQ      1
     C                   Z-ADD     2             VIEW#
     C                   ENDIF
      ***** EBD ADD ***** 04/06/04 **************************************
      *
     C     Q1PRMS        IFGT      5
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C                   ENDIF                                                  Q1PRMS>5
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C   22KEY1B         SETGT     DBRCDF2                            96
     C*
      /Free
       //***** Beg Add ***** 02/12/16 *********************************
       If *in22 = '1';
          svCer# = kkcer#;
          Readpe key1B DBRCDF2;
          If %found and (svCer# <> CEcer#);
             Dow (svcer# <> cecer#);
               Readpe key1B DBRCDF2;
             Enddo;
          Else;
      /End-free
     C   22KEY1B         SETGT     DBRCDF2                            96
      /Free
          Endif;
       Endif;
       //***** End Add ***** 02/12/16 *********************************
      /End-free

     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C   22KEY1B         SETLL     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL     DELETD        SCDATA
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF YEARS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     CESTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
      *
      ***** BEG ADD ***** 02/07/06 **************************************
     C*===============================================================
     C* SRRSFL - Validates/retrieves the remark code to use
     C*===============================================================
     C     SRRSFL        BEGSR
      *
      * Allow the user to select a remark code from a list
      *
     C                   SETOFF                                       737475
     C     *IN10         IFEQ      *ON
     C     HFLD          ANDEQ     'SCRMCD'
     C                   CALL      'SC580R'
     C                   PARM      *BLANKS       QQRMCD            5
     C                   PARM                    QQRTC             1
      *
     C     QQRMCD        IFNE      *BLANKS
     C                   MOVEL     QQRMCD        SCRMCD
     C                   ENDIF
      *
     C                   SETON                                        90
     C                   ENDIF
      *
      * Resolve flag must be 'Y/N'
     C     SCRSFL        IFNE      'Y'
     C     SCRSFL        ANDNE     'N'
     C                   SETON                                        73  90
     C                   ENDIF
      *
      * If remark code is empty, then default the flag to 'N'
     C     SCRMCD        IFEQ      *BLANKS
     C     SCRSFL        ANDEQ     'Y'
     C     #RSFL         ANDNE     'N'
     C                   MOVEL     'N'           SCRSFL
     C                   SETON                                        90
     C                   ENDIF
      *
     C                   MOVEL     SCRSFL        #RSFL
      *
      * Remark Code must be valid and active
      *
     C     SCRSFL        IFEQ      'Y'
      *
     C     SCRMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        75  90
     C                   ENDIF                                                  CTSTAT=I
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   ENDIF                                                  SRRSFL
     C*
     C                   ENDSR
      ***** END ADD ***** 02/07/06 **************************************
** Headers - HD
Opt Cert#  Name                            Remarks 1                   Rlv Sts
Opt Name                            Cert#  Remarks 1                   Rlv Sts
Opt Cert#  Name                            Remarks 2                   Rlv Sts
Opt Name                            Cert#  Remarks 2                   Rlv Sts
Opt Cert#  Name                            Remarks 3                   Rlv Sts
Opt Name                            Cert#  Remarks 3                   Rlv Sts
Opt Cert#  Name                            Remarks 4                   Rlv Sts
Opt Name                            Cert#  Remarks 4                   Rlv Sts
Opt Cert#  Name                            Remarks 5                   Rlv Sts
Opt Name                            Cert#  Remarks 5                   Rlv Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
11515=Rollover              YNN
11818=Print Certificate     YYN
12020=Activate/Inactivate   YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change Info           YYY
211F11=Change View          YYY
212F12=Cancel               YYY
214F14=Darts                YYN
215F15=Rollover All         YNN
217F17=Subset               YYY
218F18=Print All Certs      YYN
219F19=Print Blank Certs    YYN
3
