     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0395R - WORK WITH BILL STATUS HISTORY         C:62 06/22/98 *
     H*                                                               *
     H*  CALLED FROM: F0350R                                          *
     H*  CALLS TO:    F0395R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:212  Project : SY420                             *
     H*                   Mask SSN                                    *
     H*                   Recompile only                              *
     H*                                                               *
     H*  02/12/18  C:079  Support: 114/64080                          *
     H*                   Remove edit on recon for submitted bills    *
     H*                                                               *
     H*  01/28/16  C:079  Support: 114/39393                          *
     H*                   User open the display file if mode = H      *
     H*                                                               *
     H*  02/26/14  C:296  Project: NC034                              *
     H*                   Add Recon for state NC.                     *
     H*                                                               *
     H*  10/07/05  C:079  SUPPORT: MC129 49310                        *
     H*                   SEE IF CALLED FROM F0398R(PENDED PBILLS)    *
     H*                                                               *
     H*  08/08/05  C:079  PROJECT: 135MET                             *
     H*                   ADD ADDITIONAL CRITERIA FOR RECON EDI ERROR *
     H*                                                               *
     H*  04/15/04  C:079  PROJECT: FL046                              *
     H*                   1. USE GENERIC TABLE FOR SCTAPE             *
     H*                   2. ADD RECON EDITS                          *
     H*                                                               *
     H*  11/06/03  C:079  PROJECT FS269                               *
     H*                   ADD POP UP WDW FOR PREV CHECK REQUESTS      *
     H*                                                               *
     H*  02/08/02  C:062  PROJECT TX017                               *
     H*                   ADD EDIT THAT KEEPS A BILL FROM BEING       *
     H*                   RECONSIDERED IF THE BILL IS AWAITING        *
     H*                   RESUBMISSION TO THE STATE.                  *
     H*                                                               *
     H*  03/13/01  C:062  PROJECT:MCMNT                               *
     H*                   GET CORRECT RCN # FOR ESRCN EVERY TIME      *
     H*                                                               *
     H*  12/01/00  C:079  PROJECT:FSMNT                               *
     H*                   GET CORRECT RCN # FOR ESRCN                 *
     H*                                                               *
     H*  04/11/00  C:117  PROJECT:FS211
     H*                   ADDITIONAL EDITS TO PBILLS PGMS FOR DATA
     H*                   SUBMISSION
     H*
     H*  12/02/99 C:117 PROJECT: MC044   SEQ#: 20586                  *
     H*                 ADD LOGIC TO POSITION CURSOR TO TYPE WHEN     *
     H*                 DELETEING AN EOMB                             *
     H*                                                               8
     H*  09/10/99 C:56 Project: KY010                                 *
     H*                Add conditions for two new bill status types   *
     H*                'E' and 'B'
     H*                                                               *
     H*  08/24/99 C:56 Project: MC152 - Base Change                   *
     H*             1. Switch order of 'sent' and 'rcvd' dates.       *
     H*             2. Make sent date optional unless adj state       *
     H*                is Kentucky.
     H*                                                               *
     H*  07/16/99 C:62 Project: MC208                                 *
     H*           Add INITIAL DATE, TIME AND USER TO SCREEN.          *
     H*                                                               *
     H*  07/01/99 C:56 Project: MC202                                 *
     H*           Add entry parm for request letter number            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     F*** Beg Chg *** 01/28/16 **************************************
     F*********F0395F01  CF   E             WORKSTN
     FF0395F01  CF   E             WORKSTN usropn
     F*** End Chg *** 01/28/16 **************************************
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAESP     UF A E             DISK
     F                                     RENAME(PES01:DBRCDF)
     FFAEOP     IF   E             DISK
     FFTBSP     IF   E             DISK
     FFTBSL     IF   E           K DISK
     F                                     RENAME(PBS01:PBS02)
     FFTGTL     IF   E           K DISK
     F*** BEG ADD *** 04/11/00 ****************************************
     FFAESL5    UF   E           K DISK
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 04/11/00 ****************************************
     F*** BEG ADD *** 04/15/04 ****************************************
     FFDCAL5    IF   E           K DISK
     F*** END ADD *** 04/15/04 ****************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Bill Status History')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D RESMSG          C                   CONST('*** BACKED OUT ***')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
 P1  C     *INKC         PARM      *INKC         @@KC              1
 P2  C     *INKL         PARM      *INKL         @@KL              1
 P3  C                   PARM                    @@MODE            1
 P4  C                   PARM                    @@RRN             9 0
 P5  C                   PARM                    @@RRN2            9 0
 P6  C                   PARM                    @@BLST            1
 P7  C                   PARM                    @@STCD            3
 P8  C                   PARM                    @@REAS           60
 P9  C                   PARM                    @@PGM             8
     C*** BEG ADD *** 07/01/99 ****************************************
 P10 C                   PARM                    @@LTR#            9 0
     C*** END ADD *** 07/01/99 ****************************************
     C*** BEG ADD *** 09/10/99 ****************************************
 P11 C                   PARM                    @@EDI             1
     C*** END ADD *** 09/10/99 ****************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     BSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C                   KFLD                    KKRCN
     C*
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C*
     C* key list for EOMB Header Amount Transaction File
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*** BEGIN ADD *** 02/08/02 **************************************
     C     EAKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*** END   ADD *** 02/08/02 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C* key list for EOMB Detail Charges File
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 04/11/00 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    BSCO#         KKCO#
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    ESRRDT        HLRRDT
     C     *LIKE         DEFINE    ESRSDT        HLRSDT
     C     *LIKE         DEFINE    ESRSDT        HLSYDT
     C*** BEG ADD *** 04/11/00 ****************************************
     C     *LIKE         DEFINE    ESSUB#        KKSUB#
     C     *LIKE         DEFINE    ESFND         KKFND
     C     *LIKE         DEFINE    ESFYR         KKFYR
     C     *LIKE         DEFINE    ESCASE        KKCASE
     C     *LIKE         DEFINE    ESEOB#        KKEOB#
     C     *LIKE         DEFINE    ESRCN         KKRCN
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*** BEG ADD *** 04/15/04 ****************************************
     C*================================================================
     C* CHKDTL - Check FDCAP/CAPAID = 0
     C*================================================================
     C     CHKDTL        BEGSR
     C*
     C                   MOVE      'N'           XXDNY             1
     C     CAKY          CHAIN     PCA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     CAPAID        IFEQ      0
     C                   MOVE      'Y'           XXDNY
     C                   LEAVE
     C                   ENDIF
     C*
     C     CAKY          READE     PCA01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
     C* STATUS TYPE CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   MOVEL(P)  SCBLST        KKTYPE
     C                   MOVEL(P)  SCSTCD        KKCODE
     C*
     C     BSKY          CHAIN     PBS02                              99
     C*
     C   99              SETON                                        7190
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/10/99 ****************************************
     C*
     C* IF @@BLST ENTRY PARM IS 'B', THEN THE USER WILL HAVE TO CHOOSE
     C* BETWEEN DOING A RECONSIDERATION OR CHANGES FOR DATA SUBMISSION
     C*                      (R)                         (E)
     C*
     C     @@BLST        IFEQ      'B'
     C     SCBLST        IFNE      'R'
     C     SCBLST        ANDNE     'E'
     C                   SETON                                        8590
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCBLST <> R
     C                   ENDIF                                                  @@BLST = B
     C*** END ADD *** 09/10/99 ****************************************
     C*** BEGIN ADD *** 02/08/02 **************************************
     C*
     C* KEEP FROM RECONSIDERING THE BILL IF BILL IS AWAITING
     C* RESUBMISSION TO THE STATE OR WITHDRAWAL FROM THE STATE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCBLST        IFEQ      'R'
     C     EAKY1         CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C     EOAST         ANDEQ     'TX'
     C     EAEDI         ANDEQ     'R'
     C*** BEG ADD *** 04/15/04 ****************************************
     C     *IN99         OREQ      *OFF
     C     EOAST         ANDEQ     'FL'
     C     EOTAPE        ANDEQ     'R'
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 02/26/14 ****************************************
     C     *IN99         OREQ      *OFF
     C     EOAST         ANDEQ     'NC'
     C     EAEDI         ANDEQ     'R'
     C*** END ADD *** 02/26/14 ****************************************
     C                   SETON                                        8990
     C                   ENDIF
     C*** BEG ADD *** 04/15/04 ****************************************
     C     *IN99         IFEQ      *OFF
     C     EOAST         ANDEQ     'FL'
     C     EOTAPE        ANDEQ     'W'
     C                   SETON                                        8890
     C                   ENDIF
     C*
     C* RECON NOT ALLOWED FOR NEW EDI BILLS THAT HAVE DENIED LINES
     C*
     C*** Beg Del *** 02/12/18 *************************************
     C***
     C***  *IN99         IFEQ      *OFF
     C***  EOAST         ANDEQ     'FL'
     C***  EOCNTL        ANDNE     *BLANKS
     C***  EOTAPE        ANDNE     'Y'
     C*** End Del *** 02/12/18 *************************************
     C*** BEG ADD *** 08/08/05 ****************************************
     C*** Beg Del *** 02/12/18 *************************************
     C***  EOTAPE        ANDNE     'N'
     C*** End Del *** 02/12/18 *************************************
     C*** END ADD *** 08/08/05 ****************************************
     C*** Beg Del *** 02/12/18 *************************************
     C***                EXSR      CHKDTL
     C***  XXDNY         IFEQ      'Y'
     C***                EXFMT     SCRWDW
     C***                SETON                                        90
     C***                EXSR      PGMEND
     C***                ENDIF
     C***                ENDIF
     C*** End Del *** 02/12/18 *************************************
     C*** END ADD *** 04/15/04 ****************************************
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C* IF STATUS IS 'R', RECON RECEIVED AND RCON SENT DATE CANNOT BE
     C* ZERO
     C*
     C*** BEG DEL *** 08/24/99 ****************************************
     C********** *IN90     IFEQ *OFF
     C********** SCBLST    IFEQ 'R'
     C********** SCRSDT    ANDEQ*ZEROS
     C**********           SETON                     9076
     C**********           ENDIF
     C**********           ENDIF
     C*** END DEL *** 08/24/99 ****************************************
     C*** BEG ADD *** 08/24/99 ****************************************
     C     *IN90         IFEQ      *OFF
     C     SCBLST        IFEQ      'R'
     C     SCRSDT        ANDEQ     *ZEROS
     C     EOAST         ANDEQ     'KY'
     C                   SETON                                        9076
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 08/24/99 ****************************************
     C*
     C     *IN90         IFEQ      *OFF
     C     SCBLST        IFEQ      'R'
     C     SCRRDT        ANDEQ     *ZEROS
     C                   SETON                                        9075
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD *** 09/10/99 ****************************************
     C*
     C     *IN90         IFEQ      *OFF
     C     SCBLST        IFNE      'R'
     C     SCRSDT        ANDNE     *ZEROS
     C                   SETON                                        9087
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCBLST        IFNE      'R'
     C     SCRRDT        ANDNE     *ZEROS
     C                   SETON                                        9086
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 09/10/99 ****************************************
     C*
     C* DATE RECON RECIEVED MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCRRDT        IFNE      *ZEROS
     C     *IN16         OREQ      *ON
     C*
     C                   Z-ADD     SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9073
     C                   ELSE
     C                   MOVEL     SRDTO         SCRRDT
     C                   Z-ADD     SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRRDT
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCRRDT NE *ZEROS
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*
     C*
     C* Received date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDLT     EOACDT
     C                   SETON                                        9077
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDLT     EOTODT
     C*** BEG ADD *** 04/11/00 ****************************************
     C     EOTAPE        ANDNE     'Y'
     C*** END ADD *** 04/11/00 ****************************************
     C                   SETON                                        9078
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDGT     HLSYDT
     C                   SETON                                        9079
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be greater than sent date
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRRDT        IFNE      *ZEROS
     C     SCRSDT        ANDNE     *ZEROS
     C*
     C*
     C     HLRSDT        IFGT      HLRRDT
     C                   SETON                                        9072
     C                   ENDIF                                                  HLRRDT GT HLRSDT
     C*
     C                   ENDIF                                                  SCRRDT NE *ZEROS
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSDT        ANDNE     *ZEROS
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9083
     C**********           GOTO CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCRSDT
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRSDT
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDLT     EOACDT
     C                   SETON                                        9080
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDLT     EOTODT
     C                   SETON                                        9081
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDGT     HLSYDT
     C                   SETON                                        9082
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* EXPLANATION CAN'T BE BLANKS.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCRSDT        IFNE      *ZEROS
     C     SCBLST        OREQ      'R'
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRSDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        74
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR
     C*** BEG ADD *** 11/06/03 ****************************************
     C*================================================================
     C* CHKPMT - Check for an uprocessed payment at a previous recon
     C*================================================================
     C     CHKPMT        BEGSR
     C*
     C* LOAD PARM FIELDS WITH INFO FROM THIS BILL
     C*
     C                   MOVE      EACO#         #1CO#
     C                   MOVE      EASUB#        #1SUB#
     C                   MOVE      EAFND         #1FND
     C                   MOVE      EAFYR         #1FYR
     C                   MOVE      EACASE        #1CASE
     C                   MOVE      EAEOB#        ##EOB#
     C                   MOVE      EARCN         ##RCN#
     C                   MOVE      EAFEIN        ##FEIN
     C                   MOVE      EAMOD         ##MOD
     C*
     C* CHECK IF OUTSIDE ACCOUNT
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANK        ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM      *BLANK        ##OUTS            1
     C*
     C     ##WCIN        IFEQ      'Y'
     C     ##OUTS        ANDNE     'Y'
     C     ##NPWC        ANDNE     'Y'
     C     ##WCIF        ANDEQ     'Y'
     C*
     C* GET WC CROSS REFERENCE FUND NUMBER
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C* CHECK FOR A PAYMENT
     C*
     C                   CALL      'WA051R'
     C                   PARM                    #1CO#             3
     C                   PARM                    #1WCF#            3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    ##EOB#            9
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      *BLANK        ##RTCD
     C                   PARM                    ##RCN#            3
     C*
     C* DISPLAY PREVIOUS PAYMENT WINDOW
     C*
     C     ##RTCD        IFEQ      'P'
     C*
     C                   MOVE      EACO#         ##CO#
     C                   MOVE      EASUB#        ##SUB#
     C                   MOVE      #1WCF#        ##FND
     C*
     C                   CALL      'FA720R'
     C                   PARM      '6'           ##MGID            1
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM      '0'           ##KL
     C                   ENDIF                                                  ##RTCD IF
     C*
     C* IF F12 WAS PRESSED RETURN TO THE CALLING PROGRAM
     C*
     C     ##KL          IFEQ      *ON
     C                   MOVE      '1'           *INKL
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDIF                                                  WCIN IFEQ Y
     C*
     C                   ENDSR
     C*** END ADD *** 11/06/03 ****************************************
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCSTCD'
     C*
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCBLST        ##TYPE            1
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PBS01                              99
     C*
     C                   MOVEL(P)  BSCODE        SCSTCD
     C                   MOVEL(P)  BSDESC        SCTDSC
     C*****                MOVELBSDESC    SCREAS    P
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 09/10/99 ****************************************
     C     HFLD          WHENEQ    'SCSTCD'
     C*** END ADD *** 09/10/99 ****************************************
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=WORK WITH PROVIDER BILL DETAIL
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL(P)  'F0353R'      ##PGM            10
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL(P)  'F0363R'      ##PGM
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL(P)  'F0373R'      ##PGM
     C                   ENDSL
     C*
     C                   Z-ADD     @@RRN2        ##RRN
     C                   MOVE      *BLANKS       ##PGM2           10
     C                   CALL      'F0390R'
     C                   PARM      'D'           ##MODE            1
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##PGM
     C                   PARM                    ##PGM2
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL(P)  ESBLST        SCBLST
     C*
     C                   Z-ADD     ESCO#         KKCO#
     C                   MOVEL(P)  'BLST'        KKTBID
     C                   MOVEL(P)  ESBLST        KKKEY
     C*** BEGIN ADD *** 07/16/99 **************************************
     C                   Z-ADD     ESIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIDT
     C*
     C                   Z-ADD     ESITIM        SCITIM
     C                   MOVEL(P)  ESIUSR        SCIUSR
     C*** END   ADD *** 07/16/99 **************************************
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              MOVEL(P)  *BLANKS       SCSDSC
     C  N99              MOVEL(P)  GTDESC        SCSDSC
     C*
     C                   MOVEL(P)  ESSTCD        SCSTCD
     C*
     C                   MOVEL(P)  ESBLST        KKTYPE
     C                   MOVEL(P)  ESSTCD        KKCODE
     C*
     C     BSKY          CHAIN     PBS02                              99
     C*
     C  N99              MOVEL(P)  BSDESC        SCTDSC
     C   99              MOVEL(P)  *BLANKS       SCTDSC
     C*
     C                   MOVEL(P)  ESREAS        SCREAS
     C*
     C                   Z-ADD     ESRRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRRDT
     C*
     C                   Z-ADD     ESRSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRSDT
     C*
     C                   ELSE
     C*
     C*** BEG ADD *** 09/10/99 ****************************************
     C     @@BLST        IFNE      'B'
     C*** END ADD *** 09/10/99 ****************************************
     C*
     C                   MOVEL(P)  @@BLST        SCBLST
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   ENDIF                                                  @@BLST <> B
     C*** END ADD *** 09/10/99 ****************************************
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   MOVEL(P)  'BLST'        KKTBID
     C                   MOVEL(P)  ESBLST        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              MOVEL(P)  *BLANKS       SCSDSC
     C  N99              MOVEL(P)  GTDESC        SCSDSC
     C*
     C                   MOVEL(P)  *BLANKS       SCSTCD
     C                   MOVEL(P)  *BLANKS       SCREAS
     C                   Z-ADD     *ZEROS        SCRRDT
     C                   Z-ADD     *ZEROS        SCRSDT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*** BEG ADD *** 04/11/00 ****************************************
     C*** BEG DEL *** 12/01/00 ****************************************
     C*********
     C***CHAIN*OUT TO FAESP TO CHECK EAEPFL FOR SAME DAY RECON
     C*********
     C*********  SCBLST    IFEQ 'R'
     C*********            MOVE 'R'       KKBLST
     C*********  ESKY1     SETGTPES01
     C*********  ESKY2     REDPEPES01                    60
     C**N60****            Z-ADDESRCN     KKRCN
     C*********  EAKY      CHAINPEA01                99
     C*********  *IN99     IFEQ *OFF
     C*********  EAEPFL    ANDEQ'P'
     C*********            MOVE 'R'       KKBLST
     C*********  ESKY      SETGTPES01
     C*********  ESKY      REDPEPES01                    60
     C*********  ESRCN     ADD  1         ESRCN
     C*********            ENDIF                            *IN99=*OFF
     C*********            ELSE
     C*********            Z-ADDEORCN     ESRCN
     C*********            ENDIF
     C*** END DEL *** 12/01/00 ****************************************
     C*** BEG ADD *** 04/11/00 ****************************************
     C*** BEG ADD *** 12/01/00 ****************************************
     C*
     C*  CHAIN OUT TO FAESP TO CHECK EAEPFL FOR SAME DAY RECON
     C*
     C     SCBLST        IFEQ      'R'
     C*
     C                   MOVE      'R'           KKBLST
     C                   Z-ADD     EORCN         KKRCN
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     EAEPFL        ANDEQ     'P'
     C*** BEG ADD *** 11/06/03 ****************************************
     C*
     C* SEE IF THERE IS AN UNPROCESSED CHECK REQUEST AT A PREV RECON
     C*
     C                   EXSR      CHKPMT
     C*** END ADD *** 11/06/03 ****************************************
     C*
     C     EARCN         ADD       1             ESRCN
     C*
     C                   ELSE
     C                   Z-ADD     EORCN         ESRCN
     C*
     C                   ENDIF                                                  *IN99=OFF
     C*
     C                   ENDIF                                                  EOBLST=R
     C*
     C     SCBLST        IFEQ      'C'
     C*** BEGIN ADD *** 03/13/01 **************************************
     C     SCBLST        OREQ      'P'
     C     SCBLST        OREQ      'U'
     C     SCBLST        OREQ      'E'
     C     SCBLST        OREQ      'D'
     C*** END   ADD *** 03/13/01 **************************************
     C                   Z-ADD     EORCN         ESRCN
     C                   ENDIF
     C*
     C*** END ADD *** 12/01/00 ****************************************
     C*
     C                   Z-ADD     EOCO#         ESCO#
     C                   Z-ADD     EOSUB#        ESSUB#
     C                   Z-ADD     EOFND         ESFND
     C                   Z-ADD     EOCASE        ESCASE
     C                   Z-ADD     EOFYR         ESFYR
     C                   Z-ADD     EOEOB#        ESEOB#
     C*
     C                   MOVEL(P)  SCBLST        ESBLST
     C                   MOVEL(P)  SCSTCD        ESSTCD
     C                   MOVEL(P)  SCREAS        ESREAS
     C*
     C                   Z-ADD     SCRRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRRDT
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRSDT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(31)
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C                   MOVE      '1'           OUTFLG
     C*** BEG ADD *** 09/10/99 ****************************************
     C     Q1PRMS        IFGE      11
     C                   MOVE      *BLANKS       @@EDI
     C     SCBLST        IFEQ      'E'
     C                   MOVE      SCBLST        @@EDI
     C                   ENDIF                                                  SCBLST = 'E'
     C                   ENDIF                                                  Q1PRMS <= 11
     C*** END ADD *** 09/10/99 ****************************************
     C*** Beg Add *** 01/28/16 ***************************************
     C                   if        @@mode <> 'H'
     C                   close     F0395F01
     C                   endif
     C*** End Add *** 01/28/16 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** BEG ADD *** 11/06/03 ****************************************
     C                   PARM      *BLANK        ##RHAB            1
     C                   PARM      *BLANK        ##MSS             1
     C                   PARM      *BLANK        ##FSCH            1
     C                   PARM      *BLANK        ##PRVB            1
     C                   PARM      *BLANK        ##NPWC            1
     C*** END ADD *** 11/06/03 ****************************************
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*** BEG ADD *** 04/11/00 ****************************************
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C                   MOVE      EOBLST        KKBLST
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   CALL      'FA707R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C     SCL3HD        PARM      *BLANKS       ##L1             78
     C*** END ADD *** 09/10/99 ****************************************
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C*
     C     @@BLST        IFEQ      'R'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/10/99 ****************************************
     C     @@BLST        IFEQ      'E'
     C                   SETON                                        17
     C                   ELSE
     C                   SETOFF                                       17
     C                   ENDIF                                                  @@BLST = 'E'
     C*** END ADD *** 09/10/99 ****************************************
     C*
     C*** BEG ADD *** 09/10/99 ****************************************
     C*
     C*  SET INDICATOR 36 ON IF @@BLST IS 'B'.  WILL BE USED TO DISPLAY
     C*  VALID OPITONS ( R OR E ).
     C*
     C     @@BLST        IFEQ      'B'
     C                   SETON                                        36
     C                   ELSE
     C                   SETOFF                                       36
     C                   ENDIF                                                  @@BLST = 'E'
     C*
     C                   CLEAR                   SCBLST
     C*** END ADD *** 09/10/99 ****************************************
     C*** BEG ADD *** 12/02/99 ****************************************
     C*** IF DELETING AN EOMB, POSITION CURSOR TO TYPE ***
     C*
     C     @@BLST        IFEQ      'D'
     C                   SETON                                        18
     C                   ELSE
     C                   SETOFF                                       18
     C                   ENDIF                                                  @@BLST = 'D'
     C*** END ADD *** 12/02/99 ****************************************
     C*** Beg Add *** 01/28/16 ***************************************
     C                   if        @@mode <> 'H'
     C                   open      F0395F01
     C                   endif
     C*** End Add *** 01/28/16 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C** @@MODE = H: WRITE BILL STATUS RECORD
     C*
     C     @@MODE        IFEQ      'H'
     C                   MOVEL     @@BLST        SCBLST
     C                   MOVEL     @@STCD        SCSTCD
     C*
     C                   MOVE      ##CO#         KKCO#
     C*
     C* IF THE STATUS REASEON IS NOT PASSED IN GET IF FROM GENERIC
     C* TABLE FILE
     C*
     C     @@REAS        IFNE      *BLANKS
     C                   MOVEL     @@REAS        SCREAS
     C                   ELSE
     C                   MOVEL(P)  'BLST'        KKTBID
     C                   MOVEL(P)  SCBLST        KKKEY
     C     GTKY          CHAIN     PGT01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCREAS
     C                   ELSE
     C                   MOVE      *BLANKS       SCREAS
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  @@REAS = *BLANKS
     C*
     C* CHAIN TO FAEOP TO GET SS#/ACDT FOR WRITER SUBROUTINE
     C*
     C     @@RRN2        CHAIN     PEO01                              98
     C*
     C                   EXSR      WRITER
     C                   RETURN
     C                   ENDIF                                                  @@MODE = 'H'
     C*
     C     @@MODE        IFEQ      'F'
     C                   EXSR      UPDRES
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDRES - Update reason on F12=Cancel from header screen
     C*================================================================
     C     UPDRES        BEGSR
     C*
     C                   MOVE      *HIVAL        KKRCN
     C                   MOVEL     'E'           KKBLST
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  99
     C*
     C                   CLEAR                   ESREAS
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     RESMSG        ESREAS
     C                   UPDATE    PES01
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INKL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ESUSER
     C                   MOVEL     Q1PGM         ESPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESDT
     C*
     C                   MOVEL     DSTIME        ESTIME
     C*
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C     SCREAS        IFNE      *BLANKS
     C                   MOVEL     SCREAS        ESREAS
     C                   ELSE
     C                   MOVEL(P)  SCBLST        KKTYPE
     C                   MOVEL(P)  SCSTCD        KKCODE
     C     BSKY          CHAIN     PBS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSDESC        ESREAS
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C                   ENDIF                                                  SCREAS NE *BLANKS
     C                   MOVEL     EOSS#         ESSS#
     C                   Z-ADD     EOACDT        ESACDT
     C*** BEG CHG *** 04/11/00 ****************************************
     C***********          Z-ADDEORCN     ESRCN
     C*** END CHG *** 04/11/00 ****************************************
     C*
     C*** BEG ADD *** 07/01/99 ****************************************
     C     Q1PRMS        IFGE      10
     C                   Z-ADD     @@LTR#        ESLTR#
     C                   ENDIF                                                  Q1PRMS >= 10
     C*** END ADD *** 07/01/99 ****************************************
     C                   MOVEL     Q1USER        ESUSER
     C                   MOVEL     Q1USER        ESIUSR
     C                   MOVEL     Q1PGM         ESPGM
     C                   MOVEL     Q1PGM         ESIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESDT
     C*
     C                   MOVEL     SRDTO         ESIDT
     C*
     C                   MOVEL     DSTIME        ESTIME
     C                   MOVEL     DSTIME        ESITIM
     C*
     C                   WRITE     DBRCDF
     C* IF THIS PROGRAM WAS CALLED FROM F0350R01 PROVIDER BILL
     C* INFORMATION, CLEAR OUT 'A' IN @@MODE.  THIS WILL ALLOW
     C* THE COPYBOOK TO LOAD *ON INTO OUTFLG WHICH WILL END
     C* THE PROGRAM.
     C*
     C     @@PGM         IFEQ      'F0350R01'
     C     @@PGM         OREQ      'F0350R'
     C*** BEG ADD *** 10/07/05 ****************************************
     C     @@PGM         OREQ      'F0358R'
     C*** END ADD *** 10/07/05 ****************************************
     C                   MOVE      *BLANKS       @@MODE
     C                   ENDIF
     C                   FEOD      FAESP
     C*
     C                   ENDSR
