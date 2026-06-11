     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     FSNDEMLF   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F*  Generic table file logical
     FWTGTL     IF   E           K DISK
     F*
     F*  User Parameter file
     FSMUPL     IF   E           K DISK
     F*

     D                 DS
     D  TIMEID                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    XXSUBJ           55
     C                   PARM                    XXEML            40
     C                   PARM                    XXATCH           40
     C                   PARM                    XXTXT           255
     C                   PARM                    XXRTCD            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Program parm list to format numeric output to character
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*  Program parm list for call to W4701C
     C*
     C     EMLPL         PLIST
     C                   PARM                    ##COMP            3
     C                   PARM                    ##STAT            1
     C                   PARM                    ##STID           14
     C                   PARM                    ##SUBJ           55
     C                   PARM                    ##EML            40
     C                   PARM                    ##TXT           255
     C*
     C*  Program parm list for call to W2493R
     C*
     C     SYSTPL        PLIST
     C                   PARM                    ##PRDS           40
     C                   PARM                    ##CALL            1
     C                   PARM                    ##TBID            4
     C                   PARM                    ##KEY            10
     C                   PARM                    ##RTCD            1
     C*
     C*
     C*  Convert date program parameter list
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
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
     C     SNDEML        PLIST
     C                   PARM                    XXSUBJ           55
     C                   PARM                    XXEML            40
     C                   PARM                    XXATCH           40
     C                   PARM                    XXTXT           255
     C                   PARM                    XXRTCD            1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*    Generic table file key list
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTCO#         KKCO#
     C     *LIKE         DEFINE    UPSTAF        KKREP
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C*
     C*
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C                   MOVE      'AREM'        KKTBID
     C                   MOVE      *BLANKS       KKKEY
     C                   MOVEL     UPSTAF        KKKEY
     C     GTKY          CHAIN     PGT01                              61
     C                   MOVEL     *BLANKS       X1EML            40
     C  N61              MOVEL     GTDESC        X1EML            40
     C*
     C*** BREAK OUT TEXT TO 5 FLDS
     C                   EXSR      BRKTXT
     C*
     C                   EXSR      $SCR04
      *
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* $SCR04 - Prompt for sending email
     C*================================================================
     C     $SCR04        BEGSR
     C*
     C*    Default the subject line and email address
     C                   MOVEL     'Subject'     S4SUBJ
     C*
     C                   MOVE      *BLANKS       S4EML
     C                   MOVE      *BLANKS       S4TXT1
     C                   MOVE      *BLANKS       S4TXT2
     C                   MOVE      *BLANKS       S4TXT3
     C                   MOVE      *BLANKS       S4TXT4
     C                   MOVE      *BLANKS       S4TXT5
     C                   MOVE      *BLANKS       S4ATCH
     C*
     C                   MOVEL     X1EML         S4EML
     C                   MOVEL     XXTXT1        S4TXT1
     C                   MOVEL     XXTXT2        S4TXT2
     C                   MOVEL     XXTXT3        S4TXT3
     C                   MOVEL     XXTXT4        S4TXT4
     C                   MOVEL     XXTXT5        S4TXT5
     C                   MOVEL     XXATCH        S4ATCH
      *
     C*
     C*
     C*    Loop to process screen
     C     *IN49         DOUNE     '1'
     C*
     C*
     C                   SETOFF                                       49
     C                   EXFMT     SCR04
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*      Process F10=Search for email address
     C     *INKJ         IFEQ      *ON
     C*
     C                   SETON                                        49
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'S4EML'
     C                   MOVEL     'AREM'        ##TBID
     C*
     C                   CALL      'W2493R'      SYSTPL
     C*
     C     ##RTCD        IFEQ      '*'
     C                   MOVEL     ##KEY         KKKEY
     C     GTKY          CHAIN     PGT01                              61
     C                   MOVEL     *BLANKS       S4EML
     C  N61              MOVEL     GTDESC        S4EML
     C                   SETON                                        46
     C                   ELSE
     C                   SETON                                        47
     C                   ENDIF                                                  ##RTCD=*
     C*
     C     HFLD          WHENEQ    'S4SUBJ'
     C                   SETON                                        44  45
     C*
     C     HFLD          WHENEQ    'S4TXT1'
     C     HFLD          OREQ      'S4TXT2'
     C     HFLD          OREQ      'S4TXT3'
     C     HFLD          OREQ      'S4TXT4'
     C     HFLD          OREQ      'S4TXT5'
     C                   SETON                                        44  46
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C*      Make sure Email address is not blank
     C     S4EML         IFEQ      *BLANK
     C                   SETON                                        49  48
     C                   ELSE
     C                   SETOFF                                       49
     C                   ENDIF                                                  S4EML=*BLANK
     C*** EDITS PASSED, MOVE SCREEN FIELDS TO PARM FIELDS
     C*
     C                   EXSR      SC2PRM
     C*
     C* * * CALL PROGRAM TO PROCESS THE REQUEST
     C*
     C                   CALL      'SNDEMLC1'    SNDEML
     C*
     C                   ENDIF                                                  *INKJ=*OFF
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDDO                                                  *IN49 NE '1'
     C*
     C                   ENDSR                                                  $SCR04
      *
     C*================================================================
     C* BRKTXT - BREAK TEXT FIELDS
     C*================================================================
     C     BRKTXT        BEGSR
     C                   MOVEL     XXTXT         XXTXT1           51
     C     51            SUBST     XXTXT:52      XXTXT2           51
     C     51            SUBST     XXTXT:103     XXTXT3           51
     C     51            SUBST     XXTXT:154     XXTXT4           51
     C     51            SUBST     XXTXT:205     XXTXT5           51
     C                   ENDSR
      *
     C*================================================================
     C* SC2PRM - SCREEN FIELDS TO PARMS
     C*================================================================
     C     SC2PRM        BEGSR
     C     S4TXT1        CAT       S4TXT2        ##TXT1          102
     C     ##TXT1        CAT       S4TXT3        ##TXT2          153
     C     ##TXT2        CAT       S4TXT4        ##TXT3          204
     C     ##TXT3        CAT       S4TXT5        ##TXT           255
     C                   MOVEL     ##TXT         XXTXT
     C                   MOVEL     S4EML         XXEML
     C                   MOVEL     S4SUBJ        XXSUBJ
     C*
     C                   MOVE      ' '           XXRTCD
     C*
     C                   ENDSR
