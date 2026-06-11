     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H/TITLE W0091R - REVISED BILLING A/R WORK WITH PROGRAM
     H*****************************************************************
     H* CALLED FROM:                                                  *
     H*    W0090R - REVISED BILLING                                   *
     H* CALLS TO:                                                     *
     H*    W0091R02 - A/R SCREEN MAINTENANCE                          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/19/13 C:65  PROJECT: 140WAU                                *
     H*                ADD IN CODING TO DISPLAY THE NEW TYPES FOR     *
     H*                DEPOSIT AND TAX ON DEPOSIT.                    *
     H*                                                               *
     H* 12/11/08 C:65  PROJECT: WC1769                                *
     H*                ADD IN CODING TO DISPLAY THE 6TH AND 7TH       *
     H*                ASSESSMENTS. STATUS VALUES TO INDICATE A/R     *
     H*                TYPE HAS BEEN CHANGED.  SUMMARIZED ASSESSMENTS *
     H*                ARE NOW LISTED WITH A STATUS OF D THRU J.      *
     H*                                                               *
     H* 05/15/07 C:184 PROJECT WC1568                                 *
     H*                Charge assessments on the same basis as premium*
     H*                installments                                   *
     H*                                                               *
     H* 01/03/05 C:136 SUPPORT                                        *
     H*                TOTAL ADJUSTMENTS WERE BEING DISPLAYED         *
     H*                INCORRECTLY FOR WEEKLY OR TWICE-MONTHLY        *
     H*                ACOUNTS.                                       *
     H*****************************************************************
     FW0091F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWAEAP     IF   E             DISK
     F                                     RENAME(PEA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWAEAL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEA01:DBRCDF)
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
      *** BEG ADD ***** 09/17/07 **************************************
     D*** BEGIN CHG ***  12/11/08  ********************************
     D*********           AD      1   5 14
     D AD              S             14    DIM(7) CTDATA PERRCD(1)
     D*** END CHG *****  12/11/08  ********************************
      *** END ADD ***** 09/17/07 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D                 DS
     D  EA                     1    108  2
     D                                     DIM(12)
     D  EAAM01                 1      9  2
     D  EAAM02                10     18  2
     D  EAAM03                19     27  2
     D  EAAM04                28     36  2
     D  EAAM05                37     45  2
     D  EAAM06                46     54  2
     D  EAAM07                55     63  2
     D  EAAM08                64     72  2
     D  EAAM09                73     81  2
     D  EAAM10                82     90  2
     D  EAAM11                91     99  2
     D  EAAM12               100    108  2
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Revised Billing Adju-
     D                                     stments')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0091R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@FYR             3
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@EMP#        KKEMP#
     C                   MOVE      @@DIV         KKDIV
     C                   MOVE      @@FYR         KKFYR
     C*
     C                   MOVE      @@FND         SCFND
     C                   MOVE      @@FYR         SCFYR
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     EMKY          CHAIN     PEM01                              60
      *
     C                   MOVEL     FNNAME        SCNAME
      *
     C                   CALL      'WA005R'
     C                   PARM      KKEMP#        XXEMP#            9 0
     C                   PARM      KKDIV         XXDIV             5 0
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    X1DIV
      *** BEG ADD ***** 09/17/07 **************************************
     C                   KFLD                    KKSTAT
      *** END ADD ***** 09/17/07 **************************************
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAEMP#        KKEMP#
     C     *LIKE         DEFINE    EADIV         KKDIV
     C     *LIKE         DEFINE    EADIV         X1DIV
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EMNAME        XXNAME
      *** BEG ADD ***** 09/17/07 **************************************
     C     *LIKE         DEFINE    EASTAT        KKSTAT
      *** END ADD ***** 09/17/07 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
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
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   Z-ADD     0             X1DIV
      *** BEG ADD ***** 09/17/07 **************************************
     C                   MOVE      *BLANKS       KKSTAT
      *** END ADD ***** 09/17/07 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCDIV         X1DIV
      *** BEG ADD ***** 09/17/07 **************************************
     C                   MOVE      SCSTAT        KKSTAT
      *** END ADD ***** 09/17/07 **************************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     EADIV         SCDIV
     C*
     C                   Z-ADD     EAPO01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPODT
     C*
     C                   Z-ADD     0             SCAMT
      *
      *** BEG ADD ***** 01/03/05 ************************************
     C     EATRDT        IFLT      55
     C     EAAM02        MULT      EATRDT        SCAMT
     C                   ADD       EAAM01        SCAMT
     C                   ELSE
      *** END ADD ***** 01/03/05 ************************************
      *
     C     1             DO        12            X                 2 0
     C                   ADD       EA(X)         SCAMT
     C                   ENDDO
      *
      *** BEG ADD ***** 01/03/05 ************************************
     C                   ENDIF
      *** END ADD ***** 01/03/05 ************************************
      *** BEG ADD ***** 09/17/07 ************************************
     C                   MOVEL(P)  EASTAT        SCSTAT
      *** END ADD ***** 09/17/07 ************************************
     C*
     C                   MOVE      *BLANKS       SCSUM
     C*
     C                   SELECT
     C     EASTAT        WHENEQ    'B'
     C                   MOVEL(P)  'BILL'        SCTYPE
     C     EASTAT        WHENEQ    'T'
     C                   MOVEL(P)  'TAX'         SCTYPE
     C     EASTAT        WHENEQ    'C'
     C                   MOVEL(P)  'BILL'        SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C     EASTAT        WHENEQ    'U'
     C                   MOVEL(P)  'TAX'         SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C*** BEGIN ADD ***  08/19/13  **********************************
     C     EASTAT        WHENEQ    'W'
     C                   MOVEL(P)  'DEPOSIT'     SCTYPE
     C     EASTAT        WHENEQ    'Y'
     C                   MOVEL(P)  'DEPOSIT TAX' SCTYPE
     C     EASTAT        WHENEQ    'X'
     C                   MOVEL(P)  'DEPOSIT'     SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C     EASTAT        WHENEQ    'Z'
     C                   MOVEL(P)  'DEPOSIT TAX' SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C*** END ADD *****  08/19/13  **********************************
      *** BEG ADD ***** 05/15/07 ************************************
      *** BEG DEL ***** 09/17/07 ************************************
      **********
     C********** EASTAT    WHEQ '1'
     C**********           MOVEL'1ST ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C********** EASTAT    WHEQ '2'
     C**********           MOVEL'2ND ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C********** EASTAT    WHEQ '3'
     C**********           MOVEL'3RD ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C********** EASTAT    WHEQ '4'
     C**********           MOVEL'4TH ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C********** EASTAT    WHEQ '5'
     C**********           MOVEL'5TH ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C********** EASTAT    WHEQ '6'
     C**********           MOVEL'1ST ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C**********           MOVEL'YES'     SCSUM     P
     C********** EASTAT    WHEQ '7'
     C**********           MOVEL'2ND ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C**********           MOVEL'YES'     SCSUM     P
     C********** EASTAT    WHEQ '8'
     C**********           MOVEL'3RD ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C**********           MOVEL'YES'     SCSUM     P
     C********** EASTAT    WHEQ '9'
     C**********           MOVEL'4TH ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C**********           MOVEL'YES'     SCSUM     P
     C********** EASTAT    WHEQ '0'
     C**********           MOVEL'5TH ASSE'SCTYPE
     C**********           CAT  'SSMENT':1SCTYPE
     C**********           MOVEL'YES'     SCSUM     P
      *** END DEL ***** 09/17/07 ************************************
      *** END ADD ***** 05/15/07 ************************************
      *** BEG ADD ***** 09/17/07 ************************************
     C     EASTAT        WHENEQ    '1'
     C                   MOVEL(P)  AD(1)         SCTYPE
     C     EASTAT        WHENEQ    '2'
     C                   MOVEL(P)  AD(2)         SCTYPE
     C     EASTAT        WHENEQ    '3'
     C                   MOVEL(P)  AD(3)         SCTYPE
     C     EASTAT        WHENEQ    '4'
     C                   MOVEL(P)  AD(4)         SCTYPE
     C     EASTAT        WHENEQ    '5'
     C                   MOVEL(P)  AD(5)         SCTYPE
     C*** BEGIN CHG ***  12/11/08  ********************************
     C********** EASTAT    WHEQ '6'
     C**********           MOVELAD,1      SCTYPE    P
     C**********           MOVEL'YES'     SCSUM     P
     C********** EASTAT    WHEQ '7'
     C**********           MOVELAD,2      SCTYPE    P
     C**********           MOVEL'YES'     SCSUM     P
     C********** EASTAT    WHEQ '8'
     C**********           MOVELAD,3      SCTYPE    P
     C**********           MOVEL'YES'     SCSUM     P
     C********** EASTAT    WHEQ '9'
     C**********           MOVELAD,4      SCTYPE    P
     C**********           MOVEL'YES'     SCSUM     P
     C********** EASTAT    WHEQ '0'
     C**********           MOVELAD,5      SCTYPE    P
     C**********           MOVEL'YES'     SCSUM     P
     C     EASTAT        WHENEQ    '6'
     C                   MOVEL(P)  AD(6)         SCTYPE
     C     EASTAT        WHENEQ    '7'
     C                   MOVEL(P)  AD(7)         SCTYPE
     C     EASTAT        WHENEQ    'D'
     C                   MOVEL(P)  AD(1)         SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C     EASTAT        WHENEQ    'E'
     C                   MOVEL(P)  AD(2)         SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C     EASTAT        WHENEQ    'F'
     C                   MOVEL(P)  AD(3)         SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C     EASTAT        WHENEQ    'G'
     C                   MOVEL(P)  AD(4)         SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C     EASTAT        WHENEQ    'H'
     C                   MOVEL(P)  AD(5)         SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C     EASTAT        WHENEQ    'J'
     C                   MOVEL(P)  AD(6)         SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C     EASTAT        WHENEQ    'K'
     C                   MOVEL(P)  AD(7)         SCTYPE
     C                   MOVEL(P)  'YES'         SCSUM
     C*** END CHG *****  12/11/08  ********************************
      *** END ADD ***** 09/17/07 ************************************
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
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
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
Opt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
** ASSESSMENT DESCRIPTIONS
1st Assessment
2nd Assessment
3rd Assessment
4th Assessment
5th Assessment
6th Assessment
7th Assessment
