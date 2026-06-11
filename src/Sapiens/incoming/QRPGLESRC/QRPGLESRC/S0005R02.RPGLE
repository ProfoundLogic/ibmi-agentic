     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 08/01/16  C:33  Project: WC2405                              *
     H*                 Add alternate number and text enabled.       *
     H*                                                              *
     H* 03/01/16  C:65  Project: SY251                               *
     H*                 Add network printer dropdown for selection   *
     H*                 of default printer for scriptura enabled     *
     H*                 processes.                                   *
     H*                                                              *
     h*  11/16/15 C:33  Project: SY285                               *
     h*                 Add Department                               *
     H*                                                              *
     h*  07/02/15 C:306 Support: SEQ86962 / 31805                    *
     h*                 Added the email validation check             *
     h*
     H* 06/20/14  C:180 SUPPORT: RED4368                             *
     H*                 Remove user profile edit for client 129      *
     H*                 for user names beginning with "#'            *
     H*                                                              *
     H* 06/10/14  C:279  114WNK                                      *
     H*                  Add UPMXRI field.                           *
     H*                                                              *
     H* 03/07/13  C:33  Project: SY266   #2524                       *
     H*                 Hold the RRN to be used after FEOD.          *
     H*                                                              *
     H* 06/10/10  C:255  PROJECT: C133WIY                            *
     H*                  Write a record in SMUHP file when any field *
     H*                  of SMUPP file is updated.                   *
     H*                                                              *
     H* 05/11/09  C:61   PROJECT: 93WHW                              *
     H*                  Add phone number to screen for update in    *
     H*                  SMUPP/UPPHNA,UPPHN# & WMUPP/UPPHNA,UPPHN#   *
     H*                                                              *
     H* 10/01/08  C:189  PROJECT: 133WFM                             *
     H*                  - CREATE TRANSACTION FILE CHANGES FOR USER  *
     H*                    PARMS                                     *
     H*                                                              *
     H* 05/08/07  C:180  SUPPORT: DR099 52306
     H*                  - MAKE DARTS SECURITY RECORD INACTIVE WHEN
     H*                    USER IN USER'S PARAMETER IS DEACTIVATED.
     H*                                                              *
     H* 04/06/07  C:53   SUPPORT: 54302                              *
     H*                  EDIT THAT THE USER PROFILE EXISTS ON THE    *
     H*                  ISERIES                                     *
     H*                                                              *
     H* 05/25/06  C:02   ADD DFT WRD PROCESSOR
     H*                                                              *
     H* 03/26/02  C:132  PROJECT: SY069                              *
     H*                  ADD EMAIL ADDRESS TO SCREEN FOR UPDATE IN   *
     H*                  SMUPP/UPMAIL & WMUPP/UPMAIL                 *
     H*
     H* 02/07/02  C:69   Support: 31176                              *
     H*                  Add logic to update WCMS record on 20=Chg   *
     H*                  Status; left out in 8/00                    *
     H*                                                              *
     H* 10/12/00  C:33   SEQ#: WA112/24022                           *
     H*                  CALL FOLLOWING SCREENS IN CORRECT MODE.     *
     H*                                                              *
     H* 08/18/00  C:35   PROJECT: SUPPORT WA112 #24022               *
     H*                  REMOVED PART OF CHANGE MADE ON 05/15/00 SO  *
     H*                  THAT MODE FED TO WCMS AND WCMC PROGRAMS IS  *
     H*                  'C' FOR CHANGE.                             *
     H*                                                              *
     H* 08/01/00 C:61  PROJECT SY041                                 *
     H*                Add 20=Chg status.                            *
     H*                                                              *
     H*  05/15/00  C:118 Project: WC577                              *
     H*                  Changed value being loaded into ##MODE from *
     H*                  'C' to QQMODE. Made display file chgs for   *
     H*                  fields in display mode.                     *
     H*
     H*  10/05/99  C:53  Support: 19886                              *
     H*                  CHANGE RR# PARM FIELD FOR CORRECT RRN#      *
     H*                                                              *
     H*  10/04/99  C:53  Support: 19886
     H*                  CHANGE CALL TO OTHER PROGRAMS WITH THE CORRECT
     H*                  MODE STATUS.
     H*
     H*   5/28/99  C:53  MC145
     H*                  ADD WCMS, WCMC AND DARTS TO WORKFLOW
     H*
     H*  10/09/98  C:69  Support: 14235                               *
     H*                  Changed program to check staff code exists   *
     H*                  on all entries                               *
     H*                                                               *
     H*  07/10/98  C:69  Support:                                     *
     H*                  Changed program to use SA005R API            *
     H*                                                               *
     H*  01/13/98  C:33  PROJECT: WC707                              *
     H*                  TESTING FROM THIS PROJECT RESULTED IN CHANGE*
     H*                  TO NOT ALLOW F3 IN DISPLAY MODE (RE-COMPILE)*
     H****************************************************************
     FS0005F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     FSMSLL2    UF   E           K DISK    USROPN
     F                                     RENAME(PSL01:PSL02)
     F*
     FSSYLF     IF   E           K DISK
     F*
     FMMOPP     IF   E             DISK
     F                                     RENAME(POP01:POP02)
     FMMOPL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMCOL     IF   E           K DISK
     F*  Physical file definition (for maintaining record)
     FSMUPP     UF A E             DISK
     F                                     RENAME(PUP01:DBRCDF)
     F                                     INFDS(DBFDS)
     FSMUHP     O    E             DISK
      /COPY SCOPYMT01D
     D DBFDS           DS
     D  DBRRN                397    400B 0
     DsaveDbRrn        s                   like(dbRrn)
     D @Iuser          S               N   INZ(*ON)
     D NVALDS          DS
     D  XXUSR                  1     10
     D  XXCO#                 11     13  0
     D  XXIPGM                14     23
     D  XXLVL                 24     26  0
     D  XXTYPE                27     27
     D  XXUSNM                28     77
     D  XXTITL                78     92
     D  XXSTAF                93     95
     D  XXOUTQ                96    105
     D  XXPRDT               106    112  0
     D  XXSTAT               113    113
     D  XXUSER               114    123
     D  XXPGM                124    133
     D  XXTIME               134    139  0
     D  XXDT                 140    146  0
     D  XXIUSR               147    156
     D  XXIDT                157    163  0
     D  XXFND                164    166  0
     D  XXFYR                167    169  0
     D  XXEMP#               170    178  0
     D  XXRAL                179    189  2
     D  XXDIV                190    194  0
     D  XXALIC               195    209
     D  XXSUB#               210    214  0
     D  XXRGN                215    216
     D  XXOFC#               217    219  0
     D  XXEXT#               220    223  0
     D  XXLOB                224    225
     D  XXPAL                226    236  2
     D  XXDT1                237    243  0
     D  XXDT2                244    250  0
     D  XXDT3                251    257  0
     D  XXDT4                258    264  0
     D  XXDT5                265    271  0
     D  XXFL1                272    272
     D  XXFL2                273    273
     D  XXFL3                274    274
     D  XXFL4                275    275
     D  XXFL5                276    276
     D  XXNU1                277    287  2
     D  XXNU2                288    298  2
     D  XXNU3                299    309  2
     D  XXNU4                310    320  2
     D  XXNU5                321    331  2
     D  XXAL1                332    341
     D  XXAL2                342    351
     D  XXAL3                352    361
     D  XXAL4                362    371
     D  XXAL5                372    381
     D  XXXCO#               382    382
     D  XXXFND               383    383
     D  XXXEMP               384    384
     D  XXMRSU               385    385
     D  XXMRGR               386    386
     D  XXMREM               387    387
     D  XXFEIN               388    396  0
     D  XXMAIL               397    456
     D  XXADR1               457    496
     D  XXADR2               497    536
     D  XXCTY                537    561
     D  XXST                 562    563
     D  XXZIP                564    572  0
     D  XXPHNA               573    575  0
     D  XXPHN#               576    582  0
     D  XXFAXA               583    585  0
     D  XXFAX#               586    592  0
     D  XXOFF#               593    597
     D PVALDS          DS
     D  @@USR                  1     10
     D  @@CO#                 11     13  0
     D  @@IPGM                14     23
     D  @@LVL                 24     26  0
     D  @@TYPE                27     27
     D  @@USNM                28     77
     D  @@TITL                78     92
     D  @@STAF                93     95
     D  @@OUTQ                96    105
     D  @@PRDT               106    112  0
     D  @@STAT               113    113
     D  @@USER               114    123
     D  @@PGM                124    133
     D  @@TIME               134    139  0
     D  @@DT                 140    146  0
     D  @@IUSR               147    156
     D  @@IDT                157    163  0
     D  @@FND                164    166  0
     D  @@FYR                167    169  0
     D  @@EMP#               170    178  0
     D  @@RAL                179    189  2
     D  @@DIV                190    194  0
     D  @@ALIC               195    209
     D  @@SUB#               210    214  0
     D  @@RGN                215    216
     D  @@OFC#               217    219  0
     D  @@EXT#               220    223  0
     D  @@LOB                224    225
     D  @@PAL                226    236  2
     D  @@DT1                237    243  0
     D  @@DT2                244    250  0
     D  @@DT3                251    257  0
     D  @@DT4                258    264  0
     D  @@DT5                265    271  0
     D  @@FL1                272    272
     D  @@FL2                273    273
     D  @@FL3                274    274
     D  @@FL4                275    275
     D  @@FL5                276    276
     D  @@NU1                277    287  2
     D  @@NU2                288    298  2
     D  @@NU3                299    309  2
     D  @@NU4                310    320  2
     D  @@NU5                321    331  2
     D  @@AL1                332    341
     D  @@AL2                342    351
     D  @@AL3                352    361
     D  @@AL4                362    371
     D  @@AL5                372    381
     D  @@XCO#               382    382
     D  @@XFND               383    383
     D  @@XEMP               384    384
     D  @@MRSU               385    385
     D  @@MRGR               386    386
     D  @@MREM               387    387
     D  @@FEIN               388    396  0
     D  @@MAIL               397    456
     D  @@ADR1               457    496
     D  @@ADR2               497    536
     D  @@CTY                537    561
     D  @@ST                 562    563
     D  @@ZIP                564    572  0
     D  @@PHNA               573    575  0
     D  @@PHN#               576    582  0
     D  @@FAXA               583    585  0
     D  @@FAX#               586    592  0
     D  @@OFF#               593    597
     d @@Rrn1          s              9
     d intMxRi         s              2  0
     d intCount        s              3  0
     d***  Begin Add  ***  11/16/15  **************************************
     d isUseDept       s              1n   inz('1')
     d***  End   Add  ***  11/16/15  **************************************
     d outMessage      s            256
     d outTitle        s             50
     d outBtnText      s             10
     d CnstMxRi        c                   const(8)

     d UpdateRecent    pr                  extpgm('STUPDRCNT')
     d   inRRN                        9
     d   inType                       1    const
     d   inQte                        1    const

     d/copy sprGenPop
     d/copy sPrGenErr
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('User Parameters')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     MNKEY         KLIST
     C                   KFLD                    SCIPGM
     C                   KFLD                    KKOPTN
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    OPOPTN        KKOPTN
     C     *LIKE         DEFINE    UPUSR         ##USR
     C     *LIKE         DEFINE    UPUSNM        ##USNM
     C     *LIKE         DEFINE    UPOUTQ        ##OUTQ
     C     *LIKE         DEFINE    UPLVL         ##LVL
     C     *LIKE         DEFINE    UPTYPE        ##TYP
     C*
     C/COPY SCOPYMT01C
     C*
      *
      *
      *================================================================
      * CHGSTS - SUBROUTINE FOR CHANGING STATUS
      *================================================================
      *
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     UPSTAT        IFEQ      'I'
     C                   MOVE      'A'           UPSTAT
      *
     C                   ELSE
      *
     C                   MOVE      'I'           UPSTAT
     C                   ENDIF                                                  UPSTAT
      *
     C                   MOVEL     Q1USER        UPUSER
     C                   MOVEL     Q1PGM         UPPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
      *
     C                   MOVEL     DSTIME        UPTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
      *
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   OPEN      SMSLL2
     C     UPUSR         DELETE    PSL02                              99
     C                   CLOSE     SMSLL2
     C                   MOVE      'Y'           ##DLT
     C                   EXSR      UPWCMS
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   EXSR      NXTPGM                                       ADD 05/28/99
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN90         IFEQ      *OFF
     C     SCUSR         ANDEQ     *BLANKS
     C                   SETON                                        7790
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCUSR         CHAIN     SMUPL                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        8490
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     SCUSR         ##USR
     C                   EXSR      GETUSP
     C                   ENDIF
      /Free
        If CLTID = '129' and %subst(SCUSR:1:1) = '#';
           eval @Iuser = *Off;
        Else;
           eval @Iuser = *ON;
        Endif;
      /End-Free
     C*
     C*
     C     *IN90         IFEQ      *OFF
     c     @Iuser        ANDEQ     *ON
     c
     C                   CALL      'SA041C'
     C                   PARM      SCUSR         QQUSR            10
     C                   PARM      ' '           QQRTN             1
     C*
     C     QQRTN         IFEQ      '1'
     C                   SETON                                        5090
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCUSNM        ANDEQ     *BLANKS
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSTAF        ANDEQ     *BLANKS
     C                   SETON                                        7390
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSTAF        CHAIN     SMUPL1                             99
     C     *IN99         IFEQ      *OFF
     C     SCUSR         IFNE      UPUSR
     C                   SETON                                        8090
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  These edits are only preformed of the user profile exists
     C*  on the AS/400.
     C*
     C     *IN30         IFEQ      *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTITL        ANDEQ     *BLANKS
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLVL         ANDEQ     *ZEROS
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDEQ     *BLANKS
     C                   SETON                                        8390
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCIPGM        ANDNE     *BLANKS
     C                   MOVEL     *BLANKS       KKOPTN
     C     MNKEY         CHAIN     MMOPL                              90
     C     *IN90         IFEQ      *ON
     C                   SETON                                        82
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCO#         CHAIN     SMCOL                              90
     C     *IN90         IFEQ      *ON
     C                   SETON                                        78
     C                   ELSE
     C     COSTAT        IFEQ      'I'
     C                   SETON                                        7890
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN06         IFEQ      *ON
     C     SCWRDP        ANDNE     *BLANKS
     C     SCWRDP        IFNE      '*MSWORD'
     C     SCWRDP        ANDNE     '*WS    '
     C                   SETON                                        8590
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C     *IN90         IFEQ      *OFF
     C     SCXCO#        ANDNE     'Y'
     C     SCXCO#        ANDNE     'N'
     C                   SETON                                        7990
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        7590
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCOUTQ        ##OBJN           10
     C                   PARM      '*OUTQ'       ##OBJT            7
     C     *IN90         PARM                    ##ERR             1
     C     *IN90         IFEQ      *ON
     C                   SETON                                        76
     C                   ENDIF
     C                   ENDIF
      /Free
          If *IN90 = *Off;
            erscmail = *Off;
             If %Len(%trim(scmail)) > 0;
               erscmail = Check_Email(scmail:msgscmail);
               If erscmail = *On;
                  *IN90 = *On;
               EndIf;
             EndIf;
          EndIf;
      /End-free
     C*
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* GETUSP - Get user profile information
     C*================================================================
     C     GETUSP        BEGSR
     C                   CALL      'SRRTVUSR'                           99
     C                   PARM                    ##USR
     C                   PARM      *BLANKS       ##USNM
     C                   PARM      *BLANKS       ##OUTQ
     C     *IN30         PARM                    ##ERR             1
      /Free
        If CLTID = '129' and %subst(##USR:1:1) = '#';
           *IN30= *OFF;
           @Iuser = *OFF;
        Endif;
      /End-free
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     UPUSR         IFNE      *BLANKS
     C                   MOVEL     UPUSR         ##USR
     C                   EXSR      GETUSP
     C                   ENDIF
     C*
     C                   MOVEL     UPUSR         SCUSR
     C                   MOVEL     UPUSNM        SCUSNM
     C                   MOVEL     UPTITL        SCTITL
     C                   MOVEL     UPSTAF        SCSTAF
     C                   Z-ADD     UPPHNA        SCAREA
     C                   MOVEL     UPPHN#        SCPH#1
     C                   MOVE      UPPHN#        SCPH#2
      /Free
       //***  Begin Add  ***  08/01/16  ***********************************
       scPhnTxt = UPphnTxt;
       scAreaAlt = %int(%subst(%editc(UPalt#:'X'):1:3));
       scAltPhn1 = %int(%subst(%editc(UPalt#:'X'):4:3));
       scAltPhn2 = %int(%subst(%editc(UPalt#:'X'):7:4));
       scAltTxt = UPaltTxt;
       //***  End   Add  ***  08/01/16  ***********************************
      /End-free
     C                   MOVEL     UPEXT#        SCEXT#
     C                   MOVEL     UPLVL         SCLVL
     C                   MOVEL     UPTYPE        SCTYPE
     C                   MOVEL     UPIPGM        SCIPGM
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRDT
     C                   MOVEL     UPOUTQ        SCOUTQ
     C                   MOVEL     UPCO#         SCCO#
     C                   MOVEL     UPXCO#        SCXCO#
      *
     C                   MOVEL     UPMAIL        SCMAIL
      *
     C                   MOVEL     UPAL4         SCWRDP
     C***  begin add  ***  03/01/16  **************************************
     C                   Move      UPPKey        scPrtKey
     C***  end add ******  03/01/16  **************************************
      /free
       scMxRi = UPmxri;
       intMxRi = UPmxri;
       //  Begin Add  ***  11/16/15  **************************************
       scDept = UPdept;
       //  End   Add  ***  11/16/15  **************************************
      /end-free
      *
     C     SCXCO#        IFEQ      *BLANKS
     C                   MOVEL     'N'           SCXCO#
     C                   ENDIF
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'A'           SCSTAT
      *
     C                   ELSE
      *
     C                   MOVEL(P)  UPSTAT        SCSTAT
     C                   ENDIF                                                  MODE=A
      *
     C     @@MODE        IFEQ      'C'
     C                   MOVEL     UPUSR         @@USR
     C                   MOVEL     UPCO#         @@CO#
     C                   MOVEL     UPIPGM        @@IPGM
     C                   MOVEL     UPLVL         @@LVL
     C                   MOVEL     UPTYPE        @@TYPE
     C                   MOVEL     UPUSNM        @@USNM
     C                   MOVEL     UPTITL        @@TITL
     C                   MOVEL     UPSTAF        @@STAF
     C                   MOVEL     UPOUTQ        @@OUTQ
     C                   MOVEL     UPPRDT        @@PRDT
     C                   MOVEL     UPSTAT        @@STAT
     C                   MOVEL     UPUSER        @@USER
     C                   MOVEL     UPPGM         @@PGM
     C                   MOVEL     UPTIME        @@TIME
     C                   MOVEL     UPDT          @@DT
     C                   MOVEL     UPIUSR        @@IUSR
     C                   MOVEL     UPIDT         @@IDT
     C                   MOVEL     UPFND         @@FND
     C                   MOVEL     UPFYR         @@FYR
     C                   MOVEL     UPEMP#        @@EMP#
     C                   MOVEL     UPRAL         @@RAL
     C                   MOVEL     UPDIV         @@DIV
     C                   MOVEL     UPALIC        @@ALIC
     C                   MOVEL     UPSUB#        @@SUB#
     C                   MOVEL     UPRGN         @@RGN
     C                   MOVEL     UPOFC#        @@OFC#
     C                   MOVEL     UPEXT#        @@EXT#
     C                   MOVEL     UPLOB         @@LOB
     C                   MOVEL     UPPAL         @@PAL
     C                   MOVEL     UPDT1         @@DT1
     C                   MOVEL     UPDT2         @@DT2
     C                   MOVEL     UPDT3         @@DT3
     C                   MOVEL     UPDT4         @@DT4
     C                   MOVEL     UPDT5         @@DT5
     C                   MOVEL     UPFL1         @@FL1
     C                   MOVEL     UPFL2         @@FL2
     C                   MOVEL     UPFL3         @@FL3
     C                   MOVEL     UPFL4         @@FL4
     C                   MOVEL     UPFL5         @@FL5
     C                   MOVEL     UPNU1         @@NU1
     C                   MOVEL     UPNU2         @@NU2
     C                   MOVEL     UPNU3         @@NU3
     C                   MOVEL     UPNU4         @@NU4
     C                   MOVEL     UPNU5         @@NU5
     C                   MOVEL     UPAL1         @@AL1
     C                   MOVEL     UPAL2         @@AL2
     C                   MOVEL     UPAL3         @@AL3
     C                   MOVEL     UPAL4         @@AL4
     C                   MOVEL     UPAL5         @@AL5
     C                   MOVEL     UPXCO#        @@XCO#
     C                   MOVEL     UPXFND        @@XFND
     C                   MOVEL     UPXEMP        @@XEMP
     C                   MOVEL     UPMRSU        @@MRSU
     C                   MOVEL     UPMRGR        @@MRGR
     C                   MOVEL     UPMREM        @@MREM
     C                   MOVEL     UPFEIN        @@FEIN
     C                   MOVEL     UPMAIL        @@MAIL
     C                   MOVEL     UPADR1        @@ADR1
     C                   MOVEL     UPADR2        @@ADR2
     C                   MOVEL     UPCTY         @@CTY
     C                   MOVEL     UPST          @@ST
     C                   MOVEL     UPZIP         @@ZIP
     C                   MOVEL     UPPHNA        @@PHNA
     C                   MOVEL     UPPHN#        @@PHN#
     C                   MOVEL     UPFAXA        @@FAXA
     C                   MOVEL     UPFAX#        @@FAX#
     C                   MOVEL     UPOFF#        @@OFF#
      *
     C                   ENDIF                                                  MODE CHECK
     C                   ENDSR
      *
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCUSR         UPUSR
     C                   MOVEL     SCUSNM        UPUSNM
     C                   MOVEL     SCTITL        UPTITL
     C                   MOVEL     SCSTAF        UPSTAF
     C                   Z-ADD     SCAREA        UPPHNA
     C                   MOVEL     SCPH#1        UPPHN#
     C                   MOVE      SCPH#2        UPPHN#
     C                   MOVEL     SCEXT#        UPEXT#
      /Free
       //***  Begin Add  ***  08/01/16  ***********************************
       UPphnTxt = scPhnTxt;
       UPalt# = scAreaAlt * 10000000;
       UPalt# = UPalt# + (scAltPhn1 * 10000);
       UPalt# = UPalt# + scAltPhn2;
       UPaltTxt = scAltTxt;
       //***  End   Add  ***  08/01/16  ***********************************
      /End-free
     C                   MOVEL     SCLVL         UPLVL
     C                   MOVEL     SCTYPE        UPTYPE
     C                   MOVEL     SCIPGM        UPIPGM
      *
     C                   MOVEL     SCMAIL        UPMAIL
     C                   MOVEL     SCWRDP        UPAL4
     C***  begin add  ***  03/01/16  **************************************
     C                   Move      scPrtKey      UPPKey
     C***  end add ******  03/01/16  **************************************
      /free
       UPmxri = scMxRi;
       clear outMessage;
       outTitle   = 'Information';
       outBtnText = 'Ok';

       If UPmxri > 0 and UPmxri > intMxRi;
          outMessage = 'When the ''' + 'Maximum # of Recent Items''' +
           ' is increased, the system will begin adding new records to the +
           grid from that point forward. It will not automatically retrieve +
           older items.';
          PopOneBtn(outMessage:outTitle:outBtnText);
       Endif;
       //  Begin Add  ***  11/16/15  **************************************
       UPdept = scDept;
       //  End   Add  ***  11/16/15  **************************************
      /end-free
      *
      *
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C                   MOVEL     SCOUTQ        UPOUTQ
     C                   MOVEL     SCCO#         UPCO#
     C                   MOVEL     SCXCO#        UPXCO#
     C*
     C     @@MODE        IFEQ      'C'
     C*
     C                   MOVEL     UPUSR         XXUSR
     C                   MOVEL     UPCO#         XXCO#
     C                   MOVEL     UPIPGM        XXIPGM
     C                   MOVEL     UPLVL         XXLVL
     C                   MOVEL     UPTYPE        XXTYPE
     C                   MOVEL     UPUSNM        XXUSNM
     C                   MOVEL     UPTITL        XXTITL
     C                   MOVEL     UPSTAF        XXSTAF
     C                   MOVEL     UPOUTQ        XXOUTQ
     C                   MOVEL     UPPRDT        XXPRDT
     C                   MOVEL     UPSTAT        XXSTAT
     C                   MOVEL     UPUSER        XXUSER
     C                   MOVEL     UPPGM         XXPGM
     C                   MOVEL     UPTIME        XXTIME
     C                   MOVEL     UPDT          XXDT
     C                   MOVEL     UPIUSR        XXIUSR
     C                   MOVEL     UPIDT         XXIDT
     C                   MOVEL     UPFND         XXFND
     C                   MOVEL     UPFYR         XXFYR
     C                   MOVEL     UPEMP#        XXEMP#
     C                   MOVEL     UPRAL         XXRAL
     C                   MOVEL     UPDIV         XXDIV
     C                   MOVEL     UPALIC        XXALIC
     C                   MOVEL     UPSUB#        XXSUB#
     C                   MOVEL     UPRGN         XXRGN
     C                   MOVEL     UPOFC#        XXOFC#
     C                   MOVEL     UPEXT#        XXEXT#
     C                   MOVEL     UPLOB         XXLOB
     C                   MOVEL     UPPAL         XXPAL
     C                   MOVEL     UPDT1         XXDT1
     C                   MOVEL     UPDT2         XXDT2
     C                   MOVEL     UPDT3         XXDT3
     C                   MOVEL     UPDT4         XXDT4
     C                   MOVEL     UPDT5         XXDT5
     C                   MOVEL     UPFL1         XXFL1
     C                   MOVEL     UPFL2         XXFL2
     C                   MOVEL     UPFL3         XXFL3
     C                   MOVEL     UPFL4         XXFL4
     C                   MOVEL     UPFL5         XXFL5
     C                   MOVEL     UPNU1         XXNU1
     C                   MOVEL     UPNU2         XXNU2
     C                   MOVEL     UPNU3         XXNU3
     C                   MOVEL     UPNU4         XXNU4
     C                   MOVEL     UPNU5         XXNU5
     C                   MOVEL     UPAL1         XXAL1
     C                   MOVEL     UPAL2         XXAL2
     C                   MOVEL     UPAL3         XXAL3
     C                   MOVEL     UPAL4         XXAL4
     C                   MOVEL     UPAL5         XXAL5
     C                   MOVEL     UPXCO#        XXXCO#
     C                   MOVEL     UPXFND        XXXFND
     C                   MOVEL     UPXEMP        XXXEMP
     C                   MOVEL     UPMRSU        XXMRSU
     C                   MOVEL     UPMRGR        XXMRGR
     C                   MOVEL     UPMREM        XXMREM
     C                   MOVEL     UPFEIN        XXFEIN
     C                   MOVEL     UPMAIL        XXMAIL
     C                   MOVEL     UPADR1        XXADR1
     C                   MOVEL     UPADR2        XXADR2
     C                   MOVEL     UPCTY         XXCTY
     C                   MOVEL     UPST          XXST
     C                   MOVEL     UPZIP         XXZIP
     C                   MOVEL     UPPHNA        XXPHNA
     C                   MOVEL     UPPHN#        XXPHN#
     C                   MOVEL     UPFAXA        XXFAXA
     C                   MOVEL     UPFAX#        XXFAX#
     C                   MOVEL     UPOFF#        XXOFF#
     C*
     C                   ENDIF                                                  MODE CHECK
     C*
     C                   ENDSR
      /free
       //*********************************************************
       //  Delete old records
       //*********************************************************
       BegSr PurgeRecords;

       If (UPmxri < intMxRi) or (intMxRi = 0 and UPmxRi < CnstMxRi);
          @@Rrn1 = '000000000';
          UpdateRecent(@@Rrn1:'P':' ');
       Endif;

       Endsr;
      /end-free
     C*================================================================
     C* NXTPGM - Processing for other PROGRAMS
     C*================================================================
     C     NXTPGM        BEGSR
     C*
     C     *IN30         IFEQ      *OFF
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           ##MODE
     C                   ELSE
     C                   MOVE      @@MODE        ##MODE
     C                   ENDIF                                                  @@MODE=A
     C*
     C* WA025R WILL UPDATE WCMS USER PARM FILE. IF WC IS INSTALLED
     C* CALL WC PGM IF IT IS THE PDS WCMS.
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C                   CALL      'W0004R02'                           99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   ELSE
     C*
     C     ##MCIF        IFEQ      'Y'
     C                   CALL      'F0004R02'    ACDPL                  99
     C                   ENDIF
     C                   ENDIF
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *INKL
     C                   EXSR      GETDTM
     C                   GOTO      ENDNXT
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     UPLVL         QQLVL
     C                   MOVEL     UPUSNM        QQUSNM
     C                   CALL      'SC030R'                             99
     C                   PARM      UPUSR         QQUSR            10
     C                   PARM                    QQLVL             3
     C                   PARM                    QQUSNM           40
     C                   PARM      @@MODE        QQCALL            1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *INKL/KC=OFF
     C*
     C     ENDNXT        ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         IFEQ      *ON
     C                   MOVEA     '000'         *IN(41)
     C                   SETON                                        90
     C                   SELECT
     C     HFLD          WHENEQ    'SCUSR'
     C                   EXSR      SRCHUS
     C     HFLD          WHENEQ    'SCIPGM'
     C                   EXSR      SRCHMN
     C     HFLD          WHENEQ    'SCCO#'
     C                   EXSR      SRCHCO
     C                   OTHER
     C                   SETON                                        81
     C                   ENDSL
     C                   ENDIF
     C*
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
     C     @@MODE        IFEQ      'C'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  UPLVL         ##LVL
     C                   MOVEL(P)  UPTYPE        ##TYP
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                   MODE
     C*
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C                   MOVE      *BLANKS       SYWCIF
     C     'SY'          CHAIN     SSY01                              99
     C*
     C                   MOVEA     '0000'        *IN(40)
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'P'
     C                   SETON                                        40
     C                   ENDIF
     C*
     C                   CALL      'DTMCHK'
     C                   PARM      *BLANKS       XX1               1
     C*
     C     XX1           IFEQ      'Y'
     C                   MOVE      *ON           *IN06
     C                   ELSE
     C                   MOVE      *OFF          *IN06
     C                   ENDIF
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  begin add  ***  03/01/16  **************************************
     C                   Call      'SA047R'
     C                   Parm                    ##Script          1
     C                   If        ##Script = 'Y'
     C                   Eval      dspScript = *on
     C                   Else
     C                   Eval      dspScript = *off
     C                   Endif
     C***  end add ******  03/01/16  **************************************
     C*
     C***  Begin Add  ***  11/16/15  **************************************
      /Free
       scDeptTip = *blanks;
       isDisDept = *off;
       If not isUseDept;
          scDeptTip = 'Not yet enabled.';
          isDisDept = *on;
       Endif;
      /End-free
     C***  End   Add  ***  11/16/15  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   EXSR      UPWCMS
     C                   EXSR      UPDART
     C                   RETURN
      *
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* SRCHCO - Company number search
     C*================================================================
     C     SRCHCO        BEGSR
     C                   CALL      'S0501R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQRTCD            1
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C     QQRTCD        WHENEQ    'B'
     C     QQCO#         OREQ      *ZEROS
     C                   MOVE      *ON           *INKL
     C     QQRTCD        WHENEQ    '*'
     C                   MOVEL     QQCO#         SCCO#
     C                   SETON                                        43
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SRCHMN - Menu search call
     C*================================================================
     C     SRCHMN        BEGSR
     C                   CALL      'M0010R'                             99
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     MMOPP                              99
     C  N99              MOVEL     OPMNAM        SCIPGM
     C  N99              SETON                                        42
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* SRCHUS - User profile search call
     C*================================================================
     C     SRCHUS        BEGSR
     C                   CALL      'SC650C'
     C                   PARM      *BLANKS       QQUSR            10
     C                   PARM      'Y'           QQREGT            1
     C                   PARM                    QQRTCD            1
     C*
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C     QQRTCD        WHENEQ    'B'
     C     QQUSR         OREQ      *BLANK
     C                   MOVE      *ON           *INKL
     C     QQRTCD        WHENEQ    '*'
     C                   MOVE      QQUSR         ##USR
     C                   EXSR      GETUSP
     C     *IN30         IFEQ      *OFF
     C                   MOVEL     ##USR         SCUSR
     C                   MOVEL     ##USNM        SCUSNM
     C                   MOVEL     ##OUTQ        SCOUTQ
     C                   SETON                                        41
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     Q1USER        UPUSER
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C                   MOVEL     DSTIME        UPTIME
     C                   UPDATE    DBRCDF
     C*
      /free
       Exsr PurgeRecords;
      /end-free
     C     PVALDS        IFNE      NVALDS
     C                   EXSR      MVFLD
     C                   WRITE     PUH01
     C                   ENDIF                                                  DS COMPARE
     C*
     C                   MOVE      *BLANKS       ##DLT
     C                   EXSR      UPWCMS
     C                   EXSR      NXTPGM                                       ADD 05/28/99
E01  C                   ENDIF
     C     UPTYPE        IFNE      ##TYP
     C     UPLVL         ORNE      ##LVL
     C                   Z-ADD     UPCO#         UHCO#
     C                   MOVEL     UPUSR         UHUSR
     C                   MOVEL     ##LVL         UHPLVL
     C                   MOVEL     ##TYP         UHPTYP
     C                   MOVEL     UPLVL         UHNLVL
     C                   MOVEL     UPTYPE        UHNTYP
     C                   MOVEL     UPUSER        UHUSER
     C                   MOVEL     UPPGM         UHPGM
     C                   Z-ADD     UPDT          UHDATE
     C                   Z-ADD     UPTIME        UHTIME
     C                   WRITE     PUH01
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPWCMS - Update WCMS user parmeters
     C*================================================================
     C     UPWCMS        BEGSR
     C*
     C     SYWCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'                                          ADD 05/28/99
     C                   CALL      'WA025R'                             99
     C                   PARM                    UPUSR
     C                   PARM                    ##DLT             1
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDART - Update DARTS Security Master.
     C*================================================================
     C     UPDART        BEGSR
     C                   OPEN      SMSLL2
     C     UPUSR         CHAIN     PSL02                              99
     C     *IN99         IFEQ      *OFF
      *
     C     SLSTAT        IFEQ      'I'
     C                   MOVE      'A'           SLSTAT
     C                   ELSE
     C                   MOVE      'I'           SLSTAT
     C                   ENDIF
      *
     C                   MOVEL     Q1USER        SLUSER
     C                   MOVEL     Q1PGM         SLPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SLDT
      *
     C                   MOVEL     DSTIME        SLTIME
     C                   UPDATE    PSL02
     C                   ENDIF
     C                   CLOSE     SMSLL2
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVE      'A'           UPSTAT
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     Q1USER        UPIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPIDT
     C                   WRITE     DBRCDF
      /Free
       saveDbRrn = dbRrn;
       Exsr PurgeRecords;
      /End-Free
     C                   FEOD      SMUPP
      /Free
       dbRrn = saveDbRrn;
      /End-Free
     C*
     C                   MOVE      *BLANKS       ##DLT
     C                   EXSR      UPWCMS
     C                   EXSR      NXTPGM                                       ADD 05/28/99
     C                   MOVE      *ON           OUTFLG                         ADD 05/28/99
     C*
     C*
     C                   ENDSR
     C*
     C     MVFLD         BEGSR
     C*
     C                   MOVEL     @@CO#         UHCO#
     C                   MOVEL     @@USR         UHUSR
     C                   MOVEL     @@LVL         UHPLVL
     C                   MOVEL     @@TYPE        UHPTYP
     C                   MOVEL     XXLVL         UHNLVL
     C                   MOVEL     XXTYPE        UHNTYP
     C                   MOVEL     @@USER        UHUSER
     C                   MOVEL     @@PGM         UHPGM
     C                   MOVEL     @@PRDT        UHDATE
     C                   MOVEL     @@TIME        UHTIME
     C                   MOVEL     @@USNM        UHPUNM
     C                   MOVEL     XXUSNM        UHNUNM
     C                   MOVEL     @@TITL        UHPTTL
     C                   MOVEL     XXTITL        UHNTTL
     C                   MOVEL     @@STAF        UHPSTF
     C                   MOVEL     XXSTAF        UHNSTF
     C                   MOVEL     @@OUTQ        UHPOTQ
     C                   MOVEL     XXOUTQ        UHNOTQ
     C                   MOVEL     @@DT          UHPPDT
     C                   MOVEL     XXDT          UHNPDT
     C                   MOVEL     @@STAT        UHPSTS
     C                   MOVEL     XXSTAT        UHNSTS
     C                   MOVEL     @@FND         UHPFND
     C                   MOVEL     XXFND         UHNFND
     C                   MOVEL     @@FYR         UHPFYR
     C                   MOVEL     XXFYR         UHNFYR
     C                   MOVEL     @@EMP#        UHPEMP
     C                   MOVEL     XXEMP#        UHNEMP
     C                   MOVEL     @@RAL         UHPRAL
     C                   MOVEL     XXRAL         UHNRAL
     C                   MOVEL     @@DIV         UHPDIV
     C                   MOVEL     XXDIV         UHNDIV
     C                   MOVEL     @@ALIC        UHPALC
     C                   MOVEL     XXALIC        UHNALC
     C                   MOVEL     @@SUB#        UHPSB#
     C                   MOVEL     XXSUB#        UHNSB#
     C                   MOVEL     @@RGN         UHPRGN
     C                   MOVEL     XXRGN         UHNRGN
     C                   MOVEL     @@OFC#        UHPOFC
     C                   MOVEL     XXOFC#        UHNOFC
     C                   MOVEL     @@EXT#        UHPEXT
     C                   MOVEL     XXEXT#        UHNEXT
     C                   MOVEL     @@LOB         UHPLOB
     C                   MOVEL     XXLOB         UHNLOB
     C                   MOVEL     @@PAL         UHPPAL
     C                   MOVEL     XXPAL         UHNPAL
     C                   MOVEL     @@DT1         UHPDT1
     C                   MOVEL     XXDT1         UHNDT1
     C                   MOVEL     @@DT2         UHPDT2
     C                   MOVEL     XXDT2         UHNDT2
     C                   MOVEL     @@DT3         UHPDT3
     C                   MOVEL     XXDT3         UHNDT3
     C                   MOVEL     @@DT4         UHPDT4
     C                   MOVEL     XXDT4         UHNDT4
     C                   MOVEL     @@DT5         UHPDT5
     C                   MOVEL     XXDT5         UHNDT5
     C                   MOVEL     @@FL1         UHPFL1
     C                   MOVEL     XXFL1         UHNFL1
     C                   MOVEL     @@FL2         UHPFL2
     C                   MOVEL     XXFL2         UHNFL2
     C                   MOVEL     @@FL3         UHPFL3
     C                   MOVEL     XXFL3         UHNFL3
     C                   MOVEL     @@FL4         UHPFL4
     C                   MOVEL     XXFL4         UHNFL4
     C                   MOVEL     @@FL5         UHPFL5
     C                   MOVEL     XXFL5         UHNFL5
     C                   MOVEL     @@NU1         UHPNU1
     C                   MOVEL     XXNU1         UHNNU1
     C                   MOVEL     @@NU2         UHPNU2
     C                   MOVEL     XXNU2         UHNNU2
     C                   MOVEL     @@NU3         UHPNU3
     C                   MOVEL     XXNU3         UHNNU3
     C                   MOVEL     @@NU4         UHPNU4
     C                   MOVEL     XXNU4         UHNNU4
     C                   MOVEL     @@NU5         UHPNU5
     C                   MOVEL     XXNU5         UHNNU5
     C                   MOVEL     @@AL1         UHPAL1
     C                   MOVEL     XXAL1         UHNAL1
     C                   MOVEL     @@AL2         UHPAL2
     C                   MOVEL     XXAL2         UHNAL2
     C                   MOVEL     @@AL3         UHPAL3
     C                   MOVEL     XXAL3         UHNAL3
     C                   MOVEL     @@AL4         UHPAL4
     C                   MOVEL     XXAL4         UHNAL4
     C                   MOVEL     @@AL5         UHPAL5
     C                   MOVEL     XXAL5         UHNAL5
     C                   MOVEL     @@XCO#        UHPCO#
     C                   MOVEL     XXXCO#        UHNCO#
     C                   MOVEL     @@XEMP        UHPXEM
     C                   MOVEL     XXXEMP        UHNXEM
     C                   MOVEL     @@MRSU        UHPMRS
     C                   MOVEL     XXMRSU        UHNMRS
     C                   MOVEL     @@MRGR        UHPMRG
     C                   MOVEL     XXMRGR        UHNMRG
     C                   MOVEL     @@MREM        UHPMRE
     C                   MOVEL     XXMREM        UHNMRE
     C                   MOVEL     @@FEIN        UHPFEI
     C                   MOVEL     XXFEIN        UHNFEI
     C                   MOVEL     @@MAIL        UHPMAL
     C                   MOVEL     XXMAIL        UHNMAL
     C                   MOVEL     @@ADR1        UHPAD1
     C                   MOVEL     XXADR1        UHNAD1
     C                   MOVEL     @@ADR2        UHPAD2
     C                   MOVEL     XXADR2        UHNAD2
     C                   MOVEL     @@CTY         UHPCTY
     C                   MOVEL     XXCTY         UHNCTY
     C                   MOVEL     @@ST          UHPSTA
     C                   MOVEL     XXST          UHNSTA
     C                   MOVEL     @@ZIP         UHPZIP
     C                   MOVEL     XXZIP         UHNZIP
     C                   MOVEL     @@PHNA        UHPPHA
     C                   MOVEL     XXPHNA        UHNPHA
     C                   MOVEL     @@PHN#        UHPPHN
     C                   MOVEL     XXPHN#        UHNPHN
     C                   MOVEL     @@FAXA        UHPFXA
     C                   MOVEL     XXFAXA        UHNFXA
     C                   MOVEL     @@FAX#        UHPFX#
     C                   MOVEL     XXFAX#        UHNFX#
     C                   MOVEL     @@OFF#        UHPOFL
     C                   MOVEL     XXOFF#        UHNOFL
     C*
     C                   ENDSR
