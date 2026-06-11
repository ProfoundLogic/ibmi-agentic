     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F2385R - WORK WITH BILL ENTRY ERROR MESSAGES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the Bill Entry      **
     H**     Error Message file.                                     **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     ______ - Work with State Codes                          **
     H**     MENU   - 'Work with Pre-Cert/Bill Entry Error Messages' **
     H**   CALLS TO:                                                 **
     H**     F2385R01 - Work With Bill Entry Mesages                 **
     H**     F2493R - Generic Table Search                           **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   10/28/97      PROJECT: FS139                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  08/03/16 C:079 Support: 42691/099
     H*                 Make several fixes to the screen
     H*
     H*  10/17/02 C:079 PROJECT FS239
     H*                 MOVE POPULATING OF MESSAGE# TO SLCRCD SUBR
     H*
     H*  10/24/01 C:062 PROJECT FSMNT #160
     H*                 ADD F21=PRINT LIST
     H*
     H*  01/23/01  C: 117  PROJECT: FSMNT #151
     H*                    HAD TO MAKE A CHANGE TO *ENTRY PARMS
     H*
     H* 12/14/00 C:079  PROJECT: FS218
     H*                 Add STSTP and logic to chain to get the state*
     H*
     H*  10/07/99  C:117  PROJECT: NOT SURE
     H*                   CHANGING PRIMARY LOGICAL TO KEY OFF ST
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      01   Used on the PAGEDOWN keyword
     H**      02   Used on the PAGEUP keyword
     H**      03   Used to condition SFLCLR
     H**     N03   Used to condition SFLDSPCTL
     H**      04   Used to condition SFLDSP
     H**      05   Used to condition DSPATR(RI) on the sel field
     H**     N06   Used to condition DSPATR(PC) on the pos to field
     H**      07   Used to condition DSPATR(PC) on the sel field
     H**      08   Used to condition SFLNXTCHG
     H**      09   Reserved for maintenance/inquiry conditions
     H**     N10   Used to condition SFLEND (on message subfile)
     H**      11   Used to indicate the list is subsetted
     H**      12   Used to condition SFLDROP/SFLFOLD
     H**      13   Used to condition SFLCLR (on confirm delete)
     H**     N13   Used to condition SFLDSPCTL (on confirm delete)
     H**      14   Used to condition SFLDSP (on confirm delete)
     H**      15   Used to condition SFLEND (on confirm delete)
     H**      21   View 1 - Error Message sequence
     H**      22   View 2 - Error Description Sequence
     H**      23 - 29 are reserved for other views
     H**      41   Call was for State Message update
     H**      60   Used for chaining
     H**      71   Reserved for screen error indicator
     H**      72   MCR2383 Error - Invalid value (Status)
     H**      73 - 89 are reserved for screen error indicators
     H**      90   Used as general purpose error indicator
     H**      91   Used to display CPF5203; roll up/down past last rcd
     H**      92   Used to condition SFLMSGID for more than one opt 1
     H**      95   Used to condition SFLEND; also used to indicate EOF
     H**           and BOF on the file being read.
     H**      99   Used for misc purpose; error on calls, on CHAINs
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FTMIP  (PFRCDF) Bill Entry Error Message               **
     H**      FTMIL  (DBRCDF) Bill Entry Error Message (MSG#/MSEQ)   **
     H**      FTMIL1 (DBRCD2) Bill Entry Error Message (DESC/MSEQ)   **
     H**      STABL  (PST01)  System Wide State Code Table (KEY)     **
     H*****************************************************************
     H*
     FF2385F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFTMIP     IF   E             DISK
     F                                     RENAME(PMI01:PFRCDF)
     F*
     F*** BEG CHG *** 10/07/99 ****************************************
     F*  Primary logical file definition (for displaying items)
     F********FTMIL   IF  E           K        DISK
     FFTMIL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PMI01:DBRCDF)
     F*** END CHG *** 10/07/99 ****************************************
     FFTMIL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PMI01:DBRCD2)
     FSTABL     IF   E           K DISK
     F*** BEG ADD *** 12/14/00 ****************************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     F*** END ADD *** 12/14/00 ****************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Bill Entry Error Mes-
     D                                     sages')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2385R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D*** BEGIN CHG *** 10/24/01 **************************************
     D**********    10                    C         SF1PAG
     D SF1PAG          C                   CONST(9)
     D*** END   CHG *** 10/24/01 **************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*** BEGIN ADD *** 10/24/01 **************************************
     D FLAG            C                   CONST('1              ')
     D*** END   ADD *** 10/24/01 **************************************
     D*
     D* ---------------------------------------------------------------
     D* SCDATA For message number sequence
     D* ---------------------------------------------------------------
     D*** Beg Chg *** 08/03/16 ***********************************
     D******
     D******           DS
     D**X1DATA                 1     73
     D**X1MSG#                 1      7
     D**X1DESC                 9     60
     D**X1SRC                 64     64
     D**X1STAT                70     70
     D                 DS
     D  X1DATA                 1     73
     D  X1MSG#                 1      7
     D  X1DESC                12     60
     D  X1SRC                 65     65
     D  X1STAT                71     71
     D*** End Chg *** 08/03/16 ***********************************
     D*
     D* ---------------------------------------------------------------
     D* SCDATA For Description sequence
     D* ---------------------------------------------------------------
     D*** Beg Chg *** 08/03/16 ***********************************
     D**********
     D**********       DS
     D**X2DATA**               1     73
     D**X2DESC**               1     52
     D**X2MSG#**              54     59
     D**X2SRC***              64     64
     D**X2STAT**              70     70
     D                 DS
     D  X2DATA                 1     73
     D  X2DESC                 1     46
     D  X2MSG#                49     55
     D  X2SRC                 63     63
     D  X2STAT                70     70
     D*** End Chg *** 08/03/16 ***********************************

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Blank = Maint
     C*                                                    '1' = Inquiry
     C*                                                    '2' = Search
     C                   PARM                    @@RRN             9 0
     C*** BEG ADD *** 01/23/01 ****************************************
     C                   PARM                    @@TYPE            1
     C                   PARM                    @@ST              2
     C                   PARM                    @@MSG#            7
     C*** END ADD *** 01/23/01 ****************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    QQSTCD            2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** BEG CHG *** 10/07/99 ****************************************
     C*          FTMIL  (DBRCDF)  Bill Entry Error Message (MSEQ/MSEQ)
     C********   KEY1      KLIST
     C********             KFLD           KKMSG#
     C********             KFLD           KKMSEQ
     C*          FTMIL2 (DBRCDF)  Bill Entry Error Message (MSEQ/MSEQ)
     C     KEY1          KLIST
     C                   KFLD                    KKST
     C*** BEG CHG *** 10/07/99 ****************************************
     C*
     C*          FTMIL1 (DBRCD2)  Bill Entry Error Message (DESC/MSEQ)
     C     KEY2          KLIST
     C                   KFLD                    KKDESC
     C                   KFLD                    KKMSEQ
     C*** BEG ADD *** 10/07/99 ****************************************
     C     KEY3          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKMSG#
     C*** END ADD *** 10/07/99 ****************************************
     C*
     C*          STABL   (PST01)  System Wide State Code Table (KEY)
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    MIMSG#        KKMSG#
     C     *LIKE         DEFINE    MIMSEQ        KKMSEQ
     C     *LIKE         DEFINE    MIDESC        KKDESC
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    MIDESC        U1PDSC
     C     *LIKE         DEFINE    MIDESC        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSDESC        IFNE      *BLANKS
     C     SSSTAT        ORNE      *BLANK
     C                   SETON                                        11
     C                   ENDIF                                                  DESC <> Blanks
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   DO
     C*
     C     *IN97         IFEQ      *ON
     C                   EXSR      OTHFKS
     C                   ENDIF                                                   *IN97 = *On
     C   90              LEAVE                                                   VVV Leave
     C*
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        9072
     C                   LEAVE                                                   VVV Leave
     C                   ENDIF                                                   STAT <> Blank
     C*
     C                   ENDDO                                                   DO Loop
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF                                                  CALL = '2'
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKMSG#
     C                   MOVE      *LOVAL        KKMSEQ
     C                   MOVE      *LOVAL        KKDESC
     C                   MOVE      *LOVAL        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     XXSTCD        KKST
     C   21              MOVEL     XXSTCD        KKMSG#
     C   21              MOVE      SCPST1        KKMSG#
     C   22              MOVEL     SCPST2        KKDESC
     C                   Z-ADD     0             KKMSEQ
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     XXSTCD        KKMSG#
     C                   MOVE      SCMSG#        KKMSG#
     C                   MOVEL     SCDESC        KKDESC
     C                   MOVE      SCMSEQ        KKMSEQ
     C                   ENDSR
     C*** BEGIN ADD *** 10/24/01 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVE      ##CO#         ##CO
     C*
     C                   CALL      'F7012C'
     C                   PARM                    ##CO              3
     C                   PARM      FLAG          ##FLAG           15
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C                   PARM      QQSTCD        ##ST              2
     C                   PARM      SCSTNM        ##STNM           40
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 10/24/01 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     MIMSG#        SCMSG#
     C                   MOVEL     MIMSG#        X1MSG#
     C                   MOVEL     MIMSG#        X2MSG#
     C                   MOVEL     MIMSEQ        SCMSEQ
     C                   MOVEL     MIDESC        SCDESC
     C                   MOVEL     MIDESC        X1DESC
     C                   MOVEL     MIDESC        X2DESC
     C                   MOVEL     MITYPE        X1SRC
     C                   MOVEL     MITYPE        X2SRC
     C                   MOVEL     MISTAT        X1STAT
     C                   MOVEL     MISTAT        X2STAT
     C   21              MOVEL     X1DATA        SCDATA
     C   22              MOVEL     X2DATA        SCDATA
     C*** BEG ADD *** 01/23/01 ****************************************
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELSCMSG#    @@MSG#
     C*** END DEL *** 10/17/02 ****************************************
     C*** END ADD *** 01/23/01 ****************************************
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*** BEG ADD *** 07/08/99 ****************************************
     C     *INKH         IFEQ      *ON
     C*
     C                   CALL      'F2385R02'
     C                   PARM      'A'           ##MODE            1
     C     *INKC         PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM                    ##CO#             3 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*** END ADD *** 07/08/99 ****************************************
     C*** BEGIN ADD *** 10/24/01 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 10/24/01 **************************************
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C*          Process option 20 - Change status
     C*          ------------------------------------------------------
     C     OTHOPT        BEGSR
     C     SCSEL         IFEQ      '20'
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF                                                  SEL = '20'
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     @@RRN         CHAIN     PST02                              99
     C                   MOVEL     STKEY         QQSTCD
     C*** BEG ADD *** 10/07/99 ****************************************
     C                   MOVEL     QQSTCD        KKST              2
     C*** END ADD *** 10/07/99 ****************************************
     C                   MOVEL     'PC'          XXSTCD            2
     C     QQSTCD        IFNE      *BLANKS
     C                   SETON                                        41
     C                   MOVEL     QQSTCD        XXSTCD
     C                   MOVEL(P)  QQSTCD        KKKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL     STNAME        SCSTNM
     C                   MOVEL     QQSTCD        SCSTCD
     C                   ENDIF                                                  STCD <> blanks
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*** BEG CHG *** 10/07/99 ****************************************
     C***21**********      READ DBRCDF                   95
     C***22**********      READ DBRCD2                   95
     C   21KEY1          READE     DBRCDF                                 95
     C   22              READ      DBRCD2                                 95
     C*** END CHG *** 10/07/99 ****************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*** BEG CHG *** 10/07/99 ****************************************
     C***21**********      READPDBRCDF                   95
     C***22**********      READPDBRCD2                   95
     C   21KEY1          READPE    DBRCDF                                 95
     C   22              READP     DBRCD2                                 95
     C*** END CHG *** 10/07/99 ****************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY3          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCD2                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY3          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCD2                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C     *IN21         IFEQ      *ON
     C                   MOVEL(P)  'DELETED'     X1MSG#
     C                   MOVEL(P)  *BLANKS       X1DESC
     C                   MOVEL(P)  *BLANK        X1SRC
     C                   MOVEL(P)  *BLANK        X1STAT
     C                   ENDIF                                                  *IN21 = On
     C     *IN22         IFEQ      *ON
     C                   MOVEL(P)  'DELETED'     X2DESC
     C                   MOVEL(P)  *BLANKS       X2MSG#
     C                   MOVEL(P)  *BLANK        X2SRC
     C                   MOVEL(P)  *BLANK        X2STAT
     C                   ENDIF                                                  *IN22 = On
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*** BEG ADD *** 10/17/02 ****************************************
     C                   MOVEL     SCMSG#        @@MSG#
     C*** END ADD *** 10/17/02 ****************************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     MIMSEQ        IFNE      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     *IN41         IFEQ      *ON
     C                   MOVEL     MIMSG#        XXWRK2            2
     C     QQSTCD        IFNE      *BLANKS
     C     QQSTCD        ANDNE     XXWRK2
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ELSE                                                   *IN41 = On
     C     MITYPE        IFNE      'P'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   END                                                    *IN41 = On
     C*
     C     SSDESC        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  MIDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      70            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      70            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  NRES = 0
     C                   ENDIF                                                  PDSC <> Blanks
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     MISTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt   Message ID Description                                       Source Status
Opt   Description                                     Message ID Source Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   YNN
208F8=Rollover              YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           YYN
3
